### Comparing bole volumes obtained by different formulas


### plot it for ABBA
### honer nls
bv <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HONER98) & !is.na(pp$DBH98)& pp$DBH98>0.9,]$HONER98
dbh <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HONER98) & !is.na(pp$DBH98)& pp$DBH98>0.9,]$DBH98
honer.mod <- nls(bv~a*dbh^b,start = list(a=0.005,b=2.5))

### kozak nls
bv <- pp[pp$SPECIES=="ABBA" & !is.na(pp$BV98) & !is.na(pp$DBH98)& pp$DBH98>0.9,]$BV98
dbh <- pp[pp$SPECIES=="ABBA" & !is.na(pp$BV98) & !is.na(pp$DBH98) & pp$DBH98>0.9,]$DBH98
kozak.mod <- nls(bv~a*dbh^b,start = list(a=0.5,b=2))

windows()
par(mfrow=c(1,2))
plot(pp[pp$SPECIES=="ABBA",]$DBH98,pp[pp$SPECIES=="ABBA",]$BV98,xlab="DBH98",ylab="Bole Volume (m^3)",main="ABBA Bole Volumes 1998,
Kozak Model")
curve(coef(honer.mod)[[1]]*x^coef(honer.mod)[[2]],add=T,col="blue")
curve(coef(kozak.mod)[[1]]*x^coef(kozak.mod)[[2]],add=T,col="red")

plot(pp[pp$SPECIES=="ABBA",]$DBH98,pp[pp$SPECIES=="ABBA",]$HONER98,xlab="DBH98",ylab="Bole Volume (m^3)",main="ABBA Bole Volumes 1998,
Honer Model")
curve(coef(honer.mod)[[1]]*x^coef(honer.mod)[[2]],add=T,col="blue")


### plot it for PIRU
### honer nls
bv <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HONER98) & !is.na(pp$DBH98),]$HONER98
dbh <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HONER98) & !is.na(pp$DBH98),]$DBH98
honer.mod <- nls(bv~a*dbh^b,start = list(a=0.005,b=2.5))

### kozak nls
bv <- pp[pp$SPECIES=="PIRU" & !is.na(pp$BV98) & !is.na(pp$DBH98)& pp$DBH98>0.9,]$BV98
dbh <- pp[pp$SPECIES=="PIRU" & !is.na(pp$BV98) & !is.na(pp$DBH98) & pp$DBH98>0.9,]$DBH98
kozak.mod <- nls(bv~a*dbh^b,start = list(a=0.5,b=2))

windows()
par(mfrow=c(1,2))
plot(pp[pp$SPECIES=="PIRU",]$DBH98,pp[pp$SPECIES=="PIRU",]$BV98,xlab="DBH98",ylab="Bole Volume (m^3)",main="PIRU Bole Volumes 1998,
Kozak Model")
curve(coef(honer.mod)[[1]]*x^coef(honer.mod)[[2]],add=T,col="blue")
curve(coef(kozak.mod)[[1]]*x^coef(kozak.mod)[[2]],add=T,col="red")

plot(pp[pp$SPECIES=="PIRU",]$DBH98,pp[pp$SPECIES=="PIRU",]$HONER98,xlab="DBH98",ylab="Bole Volume (m^3)",main="PIRU Bole Volumes 1998,
Honer Model")
curve(coef(honer.mod)[[1]]*x^coef(honer.mod)[[2]],add=T,col="blue")

##### Kozak vs Clark, using clark coefficients for loblolly pine instead of PIRU or ABBA #######################
####				#####
####		ABBA		#####
####				#####
### kozak nls
bv <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK98) & !is.na(pp$DBH98)& pp$DBH98>0.9,]$KOZAK98
dbh <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK98) & !is.na(pp$DBH98)& pp$DBH98>0.9,]$DBH98
kozakabba.mod <- nls(bv~a*dbh^b,start = list(a=0.005,b=2.5))

### clark nls
bv <- pp[pp$SPECIES=="ABBA" & !is.na(pp$CLARK98) & !is.na(pp$DBH98),]$CLARK98
dbh <- pp[pp$SPECIES=="ABBA" & !is.na(pp$CLARK98) & !is.na(pp$DBH98),]$DBH98
clarkabba.mod <- nls(bv~a*dbh^b,start = list(a=0.005,b=2.5))

####				#####
####		PIRU		#####
####				#####
### kozak nls
bv <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK98) & !is.na(pp$DBH98)& pp$DBH98>0.9,]$KOZAK98
dbh <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK98) & !is.na(pp$DBH98)& pp$DBH98>0.9,]$DBH98
kozakpiru.mod <- nls(bv~a*dbh^b,start = list(a=0.005,b=2.5))

### clark nls
bv <- pp[pp$SPECIES=="PIRU" & !is.na(pp$CLARK98) & !is.na(pp$DBH98),]$CLARK98
dbh <- pp[pp$SPECIES=="PIRU" & !is.na(pp$CLARK98) & !is.na(pp$DBH98),]$DBH98
clarkpiru.mod <- nls(bv~a*dbh^b,start = list(a=0.005,b=2.5))

windows()
par(mfrow=c(1,2))
plot(pp[pp$SPECIES=="PIRU",]$DBH98,pp[pp$SPECIES=="PIRU",]$KOZAK98,xlab="DBH98",ylab="Bole Volume (m^3)",main="PIRU Bole Volumes 1998,
Kozak Model Vs Clark Model")
curve(coef(clarkpiru.mod)[[1]]*x^coef(clarkpiru.mod)[[2]],add=T,col="blue")
curve(coef(kozakpiru.mod)[[1]]*x^coef(kozakpiru.mod)[[2]],add=T,col="red")

plot(pp[pp$SPECIES=="ABBA",]$DBH98,pp[pp$SPECIES=="ABBA",]$KOZAK98,xlab="DBH98",ylab="Bole Volume (m^3)",main="ABBA Bole Volumes 1998,
Kozak Model Vs Clark Model")
curve(coef(clarkabba.mod)[[1]]*x^coef(clarkabba.mod)[[2]],add=T,col="blue")
curve(coef(kozakabba.mod)[[1]]*x^coef(kozakabba.mod)[[2]],add=T,col="red")


















