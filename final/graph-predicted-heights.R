## Produces graphs showing predicted heights for the different species by year
pp <- read.csv("~/work/data/data/boles/prepped-for-bole-calculation.csv")

windows()
par(mfrow = c(2,4))
### 1998
plot(pp[pp$SPECIES=="ABBA",]$DBH98,pp[pp$SPECIES=="ABBA",]$HT98)
points(pp[pp$SPECIES=="ABBA"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="ABBA"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="SOAM",]$DBH98,pp[pp$SPECIES=="SOAM",]$HT98)
points(pp[pp$SPECIES=="SOAM"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="SOAM"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="PIRU",]$DBH98,pp[pp$SPECIES=="PIRU",]$HT98)
points(pp[pp$SPECIES=="PIRU"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="PIRU"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="BECO",]$DBH98,pp[pp$SPECIES=="BECO",]$HT98)
points(pp[pp$SPECIES=="BECO"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="BECO"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)
### 2010
plot(pp[pp$SPECIES=="ABBA",]$DBH10,pp[pp$SPECIES=="ABBA",]$HT10)
points(pp[pp$SPECIES=="ABBA"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="ABBA"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="SOAM",]$DBH10,pp[pp$SPECIES=="SOAM",]$HT10)
points(pp[pp$SPECIES=="SOAM"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="SOAM"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="PIRU",]$DBH10,pp[pp$SPECIES=="PIRU",]$HT10)
points(pp[pp$SPECIES=="PIRU"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="PIRU"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="BECO",]$DBH10,pp[pp$SPECIES=="BECO",]$HT10)
points(pp[pp$SPECIES=="BECO"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="BECO"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

windows()
par(mfrow=c(2,5))
### 1998
plot(pp[pp$SPECIES=="ACSP",]$DBH98,pp[pp$SPECIES=="ACSP",]$HT98)
points(pp[pp$SPECIES=="ACSP"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="ACSP"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="ACPE",]$DBH98,pp[pp$SPECIES=="ACPE",]$HT98)
points(pp[pp$SPECIES=="ACPE"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="ACPE"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="FAGR",]$DBH98,pp[pp$SPECIES=="FAGR",]$HT98)
points(pp[pp$SPECIES=="FAGR"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="FAGR"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="BEAL",]$DBH98,pp[pp$SPECIES=="BEAL",]$HT98)
points(pp[pp$SPECIES=="BEAL"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="BEAL"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="PRPE",]$DBH98,pp[pp$SPECIES=="PRPE",]$HT98)
points(pp[pp$SPECIES=="PRPE"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="PRPE"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

### 2010
plot(pp[pp$SPECIES=="ACSP",]$DBH10,pp[pp$SPECIES=="ACSP",]$HT10)
points(pp[pp$SPECIES=="ACSP"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="ACSP"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="ACPE",]$DBH10,pp[pp$SPECIES=="ACPE",]$HT10)
points(pp[pp$SPECIES=="ACPE"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="ACPE"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="FAGR",]$DBH10,pp[pp$SPECIES=="FAGR",]$HT10)
points(pp[pp$SPECIES=="FAGR"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="FAGR"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="BEAL",]$DBH10,pp[pp$SPECIES=="BEAL",]$HT10)
points(pp[pp$SPECIES=="BEAL"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="BEAL"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="PRPE",]$DBH10,pp[pp$SPECIES=="PRPE",]$HT10)
points(pp[pp$SPECIES=="PRPE"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="PRPE"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)
