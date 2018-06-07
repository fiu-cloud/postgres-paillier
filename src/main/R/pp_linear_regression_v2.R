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

############################
## SCENARIO   PARAMETERS  ##
############################
config <- new.env()
config$n = 100
config$alpha = 0.1

##########################
## START I/O FUNCTIONS  ##
##########################
writeFrame = function(inObj,orgName,workspace,operation,iteration) {
    path = paste(workspace,'/private',sep="")
    path = paste(path,'/',sep="")
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

readFrame = function(orgName,workspace,operation,iteration) {
    path = paste(workspace,'/private',sep="")
    path = paste(path,'/',sep="")
    path = paste(path,orgName,sep="")
    path = paste(path,'/',sep="")
    path = paste(path,operation,sep="")
    path = paste(path,'/',sep="")
    path = paste(path,iteration,sep="")
    path = paste(path,".rds",sep="")
    print(paste("lOAD ",path))
    return(readRDS(path))
}



#######################
## INIT PRIVATE DATA ##
#######################
fiu <- new.env()
fiu.alpha = 0

re_1 <- new.env()
re_1$theta1 = 0
re_1$theta2 = 0
re_1$theta3 = 0

re_2 <- new.env()
re_2$theta4 = 0


#################
## CREATE DATA ##
#################
temp <- new.env()
temp$x1 = 11 : (10 + config$n)
