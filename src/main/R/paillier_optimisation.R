# Title     : TODO
# Objective : TODO
# Created by: michaelwalker
# Created on: 6/6/18
library(tictoc)
library(homomorpheR)
keypair = PaillierKeyPair$new(modulusBits = 1024)
pubkey = keypair$pubkey
privkey = keypair$getPrivateKey()
tic("A")
for(i in 1:1000){
    pubkey$encrypt(i)
}
toc()

tic("B")
data = c(1:1000)
out = pubkey$encrypt(data)
toc()



#privkey$decrypt(pubkey$add(pubkey$encrypt(5), pubkey$mult(pubkey$encrypt(6),5)))

