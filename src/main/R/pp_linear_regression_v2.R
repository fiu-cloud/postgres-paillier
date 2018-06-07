##########################################################################################
# Distributed Privacy-Preserving Gradient Descent algorithm for Linear Regression
# KeeSiong Ng
##########################################################################################
# Basic setup:
# FIU holds the vector y of true labels
# RE1 holds the data x[,1:3]
# RE2 holds the data x[,4]
# Together, they want to learn a linear model to predict y using x but in such a way that
# - nobody sees each other's data,
# - RE1 holds the coefficients for the variables it holds, which is not visible to others
# - RE2 holds the coefficients for the variables it holds, which is not visible to others
# - Prediction of a new instance requires the collaboration of RE1 and RE2
###########################################################################################

###############################################################################################
# FIU first generates a public-private key pair using the Paillier scheme
# Public key is used by RE1 and RE2 to encrypt their data and do maths on them.
# Private key is visible only to FIU and is used by the FIU to decrypt data sent from the REs.
###########
rm(list=ls())
library(homomorpheR)
library(tictoc)

############################
## SCENARIO   PARAMETERS  ##
############################
config <- new.env()
config$n = 50
config$iterations = 100
config$alpha = 0.1
config$home = paste(Sys.getenv("HOME"),"/paillier_shared",sep="")



##########################
## PAILLIER FUNCTIONS  ##
##########################
keypair = PaillierKeyPair$new(modulusBits = 1024)
pubkey = keypair$pubkey
privkey = keypair$getPrivateKey()
# Convenience function
zipf = function(x1, x2, x3) {
    res = c()
    for (i in 1 : length(x1))res = c(res, x1[i], x2[i], x3[i])
    res
}
# Paillier is defined only on positive integers.
# We approximate a floating point number X by 3 integers (a,b,c)
# such that X is approximately equal to (a + b/largenum) / 10^c
#
# Example: Assuming largenum = 2^10, 5.2 can be represented by # (5,204,0)=(5+204/1024)/1=5.19922
# or (50, 2040, 1) = (50 + 2040/1024) / 10 = 5.19922
#
# In general, the larger largenum is, the better the approximation.
largenum = 2 ^ 50
encodeFloat = function(x, e=0) {
    x = x * 10 ^ e
    x1 = floor(x)
    x2 = x - x1
    x2 = floor(x2 * largenum)
    exps = rep(e, length(x))
    zipf(x1, x2, exps)
}
# Some useful constants
zero = pubkey$encrypt('0')
one = pubkey$encrypt('1')
# We encrypt each number (a,b,c) by encrypting a and b with the public key
# The input can be a vector of numbers
encrypt = function(z, e=0) {
    y = encodeFloat(z, e)
    res = rep(zero, length(y))
    for (i in 1 : length(y)) {
        if (i %% 3 == 0) { res[i] = y[i]}
        else { res[i] = pubkey$encrypt(as.character(y[i]))}
    }
    res}
# We decrypt each encrypted number (enc(a), enc(b),c) using the private key
# by computing (dec(enc(a)) + dec(enc(b))/largenum) / 10^c
# The input can be a vector of numbers.
# We need to deal with negative numbers too.
decrypt = function(en) {
    uen = en

    for (i in 1 : length(uen)) {
        if (i %% 3 != 0)uen[i] = privkey$decrypt(en[i])
    }
    # deal with negative results;
    # see https://crypto.stackexchange.com/questions/19457/how-can-i-do-minus-on-plaintexts-in-the-paillier-cryptosystem
    for (i in 1 : length(uen)) {
        if (i %% 3 != 0 && # don't process exponents
        uen[i] >= as.double(pubkey$n / 3.0)) { uen[i] = uen[i] -
        pubkey$n}}
    res = c()
    for (i in seq(1, length(uen), 3)) {
        res = c(res,
        (as.double(uen[i]) + as.double(uen[i + 1] / (largenum))) /
        as.double(10 ^ uen[i + 2]))
    }
    return(res)
}

# We next define addition of two encrypted numbers: (enc(a1),enc(b1), c1) and (enc(a2), enc(b2), c2).
# Addition can be done on the encrypted a's and encrypted b's if c1= c2. We make c1 = c2 true by
# multiplying by powers of 10 when necessary.
# The need to make c1 = c2 is the reason why we can't have c1 and c2 encrypted.
# The function works on vectors.
addenc = function(x, y) {
    res = x
    for (i in seq(1, length(x), 3)) {
        if (x[i + 2] != y[i + 2]) {  # if different exponent, make the exponents equal by multiplying
            res[i + 2] = max(x[i + 2], y[i + 2])
            xdiff = res[i + 2] - x[i + 2]
            ydiff = res[i + 2] - y[i + 2]
            if (xdiff > 0) {
                x[i] = pubkey$mult(x[i], 10 ^ (xdiff))
                x[i + 1] = pubkey$mult(x[i + 1], 10 ^ (xdiff))
            }
            if (ydiff > 0) {
                y[i] = pubkey$mult(y[i], 10 ^ (ydiff))
                y[i + 1] = pubkey$mult(y[i + 1], 10 ^ (ydiff))
            }
        }
        res[i] = pubkey$add(x[i], y[i])
        # print(privkey$decrypt(c(x[i], y[i], res[i])))
        res[i + 1] = pubkey$add(x[i + 1], y[i + 1])
    }
    res}

# We next define subtraction of two encrypted numbers: (enc(a1),enc(b1), c1) and (enc(a2), enc(b2), c2).
# We simply multiply the second encrypted number by -1 and then add it to the first encrypted number.
subenc = function(x, y) {
    for (i in seq(1, length(x), 3)) {
        # y[i:i+1] = pubkey$mult(y[i:i+1], -1)  # this doesn't work,why?
        y[i] = pubkey$mult(y[i], - 1)
        y[i + 1] = pubkey$mult(y[i + 1], - 1)
    }
    addenc(x, y)
}

# We next define the multiplication of an encrypted number en = (enc(a),enc(b),c) by a scalar y
# Again, Paillier only allows y to be an integer. To deal with floating point numbers, we use
# en * y = en * (y_int + y_frac)
#        = en * y_int + (enc(a) * floor(y_frac * 10^n), enc(b) * floor(y_frac * 10^n), n)
# where n is a parameter with larger n leading to better approximation. We use n = 3 below.
smultenc = function(x, y) {
    prec = 5
    yint = floor(y * 10 ^ prec)
    res = x
    for (i in seq(1, length(x), 3)) {
        res[i] = pubkey$mult(x[i], yint)
        res[i + 1] = pubkey$mult(x[i + 1], yint)
        res[i + 2] = x[i + 2] + prec
    }
    res}


##########################
##    I/O FUNCTIONS     ##
##########################
writeShared = function(inObj,orgName,workspace,operation,iteration) {
    path = paste(workspace,'/',sep="")
    path = paste(path,orgName,sep="")
    path = paste(path,'/',sep="")
    path = paste(path,operation,sep="")
    path = paste(path,'/',sep="")
    dir.create(path, showWarnings = FALSE, recursive = TRUE, mode = "0700")
    path = paste(path,iteration,sep="")
    path = paste(path,".rds",sep="")
    print(paste("SAVE ",path))
    saveRDS(inObj,file=path)
    rm(inObj)
}

readShared = function(orgName,workspace,operation,iteration) {
    path = paste(workspace,'/',sep="")
    path = paste(path,orgName,sep="")
    path = paste(path,'/',sep="")
    path = paste(path,operation,sep="")
    path = paste(path,'/',sep="")
    path = paste(path,iteration,sep="")
    path = paste(path,".rds",sep="")
    print(paste("lOAD ",path))
    return(readRDS(path))
}


#####################
## INITIALISE DATA ##
#####################
fiu <- new.env()
re_1 <- new.env()
re_2 <- new.env()
tempData <- new.env()

fiu.alpha = 0

tempData$x1 = 11 : (10 + config$n)
tempData$x1 = 11 : (10 + config$n)
tempData$x2 = runif(config$n, 5, 95)
tempData$x3 = rbinom(config$n, 1, 0.5)
tempData$x = data.frame(tempData$x1, tempData$x2, tempData$x3)
tempData$x = scale(tempData$x)
tempData$x = data.frame(1, tempData$x)
tempData$eps = rnorm(tempData$x1, 0, 1.4) #noise vector

#initialise fiu
fiu$b = c(17, - 2.5, 0.5, - 5.2) #real coefficients
fiu$labels = fiu$b[1] +
    fiu$b[2] * tempData$x[, 2] +
    fiu$b[3] * tempData$x[, 3] +
    fiu$b[4] * tempData$x[, 4] +
    scale(tempData$eps) #target variable

#Initialise data for REs
re_1$x1 = encrypt(tempData$x[, 1])
re_1$x2 = encrypt(tempData$x[, 2])
re_1$x3 = encrypt(tempData$x[, 3])
re_2$x4 = encrypt(tempData$x[, 4])
re_1$theta1 = 0
re_1$theta2 = 0
re_1$theta3 = 0
re_2$theta4 = 0

#remove temp data
rm(tempData)


#####################
## RUN EXERPIMENT  ##
#####################

re1pred <- function(iteration) {
    re1Prediction = (addenc(smultenc(re_1$x1, re_1$theta1),
    addenc(smultenc(re_1$x2, re_1$theta2), smultenc(re_1$x3, re_1$theta3))))
    tic("SAVE")
    writeShared(re1Prediction,'re1',config$home,'prediction',iteration)
    toc()
}

re2pred <- function(iteration) {
    tic("LOAD")
    partialFromRe1 = readShared('re1',config$home,'prediction',iteration)
    toc()
    re2Prediction = (addenc(partialFromRe1, smultenc(re_2$x4, re_2$theta4)))
    writeShared(re2Prediction,'re2',config$home,'prediction',iteration)
}


run <- function(iter,rate){
    for (i in 1 : iter) {
        re1pred(i)
        re2pred(i)
    }
}

run(config$iterations,config$alpha)


