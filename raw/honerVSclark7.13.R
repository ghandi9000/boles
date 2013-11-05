### comparing honers calculated volumes to clark volumes
### this version of the honer equation takes dbh in cm and height in m
### thus returning cubic meter, while the clark returns cubic ft

honer2 <- function(dbh,height,species) {
	if(species == "BECO") a0 = 2.222; a1 = 91.554; a2 = 0.0043222;
	if(species == "BEAL") a0 = 1.449; a1 = 105.081; a2 = 0.004320;
	if(species == "ACSA" | species == "ACSP" | species == "ACPE")
		a0 =1.046; a1 = 117.035; a2 = 0.004334;
	if(species == "FAGR") a0=0.959; a1 = 102.056; a2 = 0.004334
	if(species == "PRPE" | species == "SOAM") a0 = 0.033; a1 = 119.889; a2 = 0.004334;
	V = (a2*dbh^2)/(a0+(a1/height))
	return(V)
}

##################### volume columns ####################################
pp$CLARK98 <- rep(NA, nrow(pp))

for(i in 1:nrow(pp)) {
	if(!is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$HT98[[i]]>=5.28) {
		if(pp$SPECIES[[i]] == "ACSA" | pp$SPECIES[[i]] == "ACPE" | pp$SPECIES[[i]] == "ACSP" | 
		pp$SPECIES[[i]] == "BECO" | pp$SPECIES[[i]] == "BEAL" | pp$SPECIES[[i]] == "FAGR" |
		pp$SPECIES[[i]] == "PRPE" | pp$SPECIES[[i]] == "SOAM" | pp$SPECIES[[i]] == "ABBA" | 
		pp$SPECIES[[i]] == "PIRU" ) {
			pp$CLARK98[[i]] <- clark(pp$DBH98[[i]],pp$HT98[[i]],pp$SPECIES[[i]])
		}
	}
############################### Honer check 
	if(!is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$HT98[[i]]>=5.28) {
		if(pp$SPECIES[[i]] == "ACSA" | pp$SPECIES[[i]] == "ACPE" | pp$SPECIES[[i]] == "ACSP" | 
		pp$SPECIES[[i]] == "BECO" | pp$SPECIES[[i]] == "BEAL" | pp$SPECIES[[i]] == "FAGR" |
		pp$SPECIES[[i]] == "PRPE" | pp$SPECIES[[i]] == "SOAM") {
			pp$HONER98[[i]] <- honer2(pp$DBH98[[i]],pp$HT98[[i]],species = pp$SPECIES[[i]])
		}
	}
}


### plot it
### honer nls
bv <- pp[pp$SPECIES=="BECO" & !is.na(pp$HONER98) & !is.na(pp$DBH98),]$HONER98
dbh <- pp[pp$SPECIES=="BECO" & !is.na(pp$HONER98) & !is.na(pp$DBH98),]$DBH98
honer.mod <- nls(bv~a*dbh^b,start = list(a=0.5,b=2))

### clark nls
bv <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK98) & !is.na(pp$DBH98),]$CLARK98
dbh <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK98) & !is.na(pp$DBH98),]$DBH98
clark.mod <- nls(bv~a*dbh^b,start = list(a=0.5,b=2))

windows()
par(mfrow=c(1,2))
plot(pp[pp$SPECIES=="BECO",]$DBH98,pp[pp$SPECIES=="BECO",]$CLARK98,xlab="DBH98",ylab="Bole Volume (m^3)",main="BECO Bole Volumes 1998,
Clark 1991 Model, Heights >= 5.28 m")
plot(pp[pp$SPECIES=="BECO",]$DBH98,pp[pp$SPECIES=="BECO",]$HONER98,xlab="DBH98",ylab="Bole Volume (m^3)",main="BECO Bole Volumes 1998,
Honer Model, Heights >= 5.28 m")

windows()
plot(pp[pp$SPECIES=="BECO",]$DBH98,pp[pp$SPECIES=="BECO",]$CLARK98,xlab="DBH98",ylab="Bole Volume (m^3)",main="BECO Bole Volumes 1998",pch="")
points(pp[pp$SPECIES=="BECO",]$DBH98,pp[pp$SPECIES=="BECO",]$CLARK98,col="red")
points(pp[pp$SPECIES=="BECO",]$DBH98,pp[pp$SPECIES=="BECO",]$HONER98,col="blue")
curve(coef(clark.mod)[[1]]*x^coef(clark.mod)[[2]],col="red",add=T)
curve(coef(honer.mod)[[1]]*x^coef(honer.mod)[[2]],col="blue",add=T)

windows()
plot(pp[pp$SPECIES=="BEAL",]$DBH98,pp[pp$SPECIES=="BEAL",]$CLARK98,xlab="DBH98",ylab="Bole Volume (m^3)",main="BEAL Bole Volumes 1998",pch="")
points(pp[pp$SPECIES=="BEAL",]$DBH98,pp[pp$SPECIES=="BEAL",]$CLARK98,col="red")
points(pp[pp$SPECIES=="BEAL",]$DBH98,pp[pp$SPECIES=="BEAL",]$HONER98,col="blue")

windows()
plot(pp[pp$SPECIES=="ACSA",]$DBH98,pp[pp$SPECIES=="ACSA",]$CLARK98,xlab="DBH98",ylab="Bole Volume (m^3)",main="ACSA Bole Volumes 1998",pch="")
points(pp[pp$SPECIES=="ACSA",]$DBH98,pp[pp$SPECIES=="ACSA",]$CLARK98,col="red")
points(pp[pp$SPECIES=="ACSA",]$DBH98,pp[pp$SPECIES=="ACSA",]$HONER98,col="blue")


windows()
plot(pp[pp$SPECIES=="SOAM",]$DBH98,pp[pp$SPECIES=="SOAM",]$CLARK98,xlab="DBH98",ylab="Bole Volume (m^3)",main="SOAM Bole Volumes 1998",pch="")
points(pp[pp$SPECIES=="SOAM",]$DBH98,pp[pp$SPECIES=="SOAM",]$CLARK98,col="red")
points(pp[pp$SPECIES=="SOAM",]$DBH98,pp[pp$SPECIES=="SOAM",]$HONER98,col="blue")










