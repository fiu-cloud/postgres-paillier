rm(list=ls())

n = 1000000
x1 = 11 : (10 + n)
x2 = runif(n, 5, 95)
x3 = rbinom(n, 1, 0.5)
x = data.frame(x1, x2, x3)
summary(x)
writePrivate = function(inObj,orgName,workspace,operation,iteration) {
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

writePrivate(x,'re1','/Users/michaelwalker/r-files','x','0')
rm(x)

readPrivate = function(orgName,workspace,operation,iteration) {
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
y = readPrivate('re1','/Users/michaelwalker/r-files','x','0')
summary(y)