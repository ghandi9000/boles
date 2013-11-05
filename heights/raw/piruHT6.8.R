### PIRU heights 6/8
### get summary of what data is available
### combine the data from all the years to get a big dataset to get
### height functions for the different elevations

pp <- read.csv("C:/Documents and Settings/Noah/My Documents/Dropbox/Noah/canopy.csv",header=T)

### sample sizes of PIRU that have both dbh and ht for the different years

a <- nrow(pp[pp$SPECIES == "PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]) ### 176 from 1986
b <- nrow(pp[pp$SPECIES == "PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98),]) ### 512 from 1998
c <- nrow(pp[pp$SPECIES == "PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10),]) ### 145 from 2010
d <- nrow(pp[pp$SPECIES == "PIRU" & !is.na(pp$HT11) & !is.na(pp$DBH11),]) ### 0 from 2011

### sample sizes from the different years that were not previously sampled

nrow(pp[pp$SPECIES == "PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]) ### 432 from 1998 that did not have heights in 1986
nrow(pp[pp$SPECIES == "PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]) ### 21 from 2010 that did not have heights in 1986 or 1998

### total usable sample size of 629

piru <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) | ### 98 trees
	pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),] ### 2010 trees

### sample sizes for elevation:: H-132, M-341, L-112
### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT98
year <- rep(86,length(ht86))
piru <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(piru) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$DBH98
ht98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$HT98
elev98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$ELEV
plots98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$PLOT
canht98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

piru <- rbind(piru,data98)
piru <- rbind(piru,data10)

#### summary plots
par(mfrow=c(1,3))
plot(piru[piru$ELEV=="L",]$DBH,piru[piru$ELEV=="L",]$HT,pch="")
points(piru[piru$ELEV=="L"&piru$YEAR==86,]$DBH,piru[piru$ELEV=="L"&piru$YEAR==86,]$HT,col="blue")
points(piru[piru$ELEV=="L"&piru$YEAR==98,]$DBH,piru[piru$ELEV=="L"&piru$YEAR==98,]$HT,col="red")
points(piru[piru$ELEV=="L"&piru$YEAR==10,]$DBH,piru[piru$ELEV=="L"&piru$YEAR==10,]$HT,col="green")
abline(lowbelow.fit)
abline(lowabove.fit)
curve(coef(lowbelow.mod)[[1]]*x^coef(lowbelow.mod)[[2]],col="blue",add=T)
curve(coef(lowabove.mod)[[1]]*x^coef(lowabove.mod)[[2]],col="green",add=T)

plot(piru[piru$ELEV=="M",]$DBH,piru[piru$ELEV=="M",]$HT,pch="")
points(piru[piru$ELEV=="M"&piru$YEAR==86,]$DBH,piru[piru$ELEV=="M"&piru$YEAR==86,]$HT,col="blue")
points(piru[piru$ELEV=="M"&piru$YEAR==98,]$DBH,piru[piru$ELEV=="M"&piru$YEAR==98,]$HT,col="red")
points(piru[piru$ELEV=="M"&piru$YEAR==10,]$DBH,piru[piru$ELEV=="M"&piru$YEAR==10,]$HT,col="green")
abline(midbelow.fit)
abline(midabove.fit)
curve(coef(midbelow.mod)[[1]]*x^coef(midbelow.mod)[[2]],col="blue",add=T)
curve(coef(midabove.mod)[[1]]*x^coef(midabove.mod)[[2]],col="green",add=T)

plot(piru[piru$ELEV=="H",]$DBH,piru[piru$ELEV=="H",]$HT,pch="")
points(piru[piru$ELEV=="H"&piru$YEAR==86,]$DBH,piru[piru$ELEV=="H"&piru$YEAR==86,]$HT,col="blue")
points(piru[piru$ELEV=="H"&piru$YEAR==98,]$DBH,piru[piru$ELEV=="H"&piru$YEAR==98,]$HT,col="red")
points(piru[piru$ELEV=="H"&piru$YEAR==10,]$DBH,piru[piru$ELEV=="H"&piru$YEAR==10,]$HT,col="green")
abline(highbelow.fit)
abline(highabove.fit)
curve(coef(highbelow.mod)[[1]]*x^coef(highbelow.mod)[[2]],col="blue",add=T)
curve(coef(highabove.mod)[[1]]*x^coef(highabove.mod)[[2]],col="green",add=T)

### linear fits above and below canopy ##############################
lowbelow.fit <- lm(piru[piru$ELEV=="L" & piru$HT<piru$CANHT,]$HT~piru[piru$ELEV=="L" & piru$HT<piru$CANHT,]$DBH)
lowabove.fit <- lm(piru[piru$ELEV=="L" & piru$HT>=piru$CANHT,]$HT~piru[piru$ELEV=="L" & piru$HT>=piru$CANHT,]$DBH)

midbelow.fit <- lm(piru[piru$ELEV=="M" & piru$HT<piru$CANHT,]$HT~piru[piru$ELEV=="M" & piru$HT<piru$CANHT,]$DBH)
midabove.fit <- lm(piru[piru$ELEV=="M" & piru$HT>=piru$CANHT,]$HT~piru[piru$ELEV=="M" & piru$HT>=piru$CANHT,]$DBH)

highbelow.fit <- lm(piru[piru$ELEV=="H" & piru$HT<piru$CANHT,]$HT~piru[piru$ELEV=="H" & piru$HT<piru$CANHT,]$DBH)
highabove.fit <- lm(piru[piru$ELEV=="H" & piru$HT>=piru$CANHT,]$HT~piru[piru$ELEV=="H" & piru$HT>=piru$CANHT,]$DBH)

############# nonlinear fits above and below canopy ##########################
lowbelowht <- piru[piru$ELEV=="L" & piru$HT<piru$CANHT,]$HT
lowbelowdbh <- piru[piru$ELEV=="L" & piru$HT<piru$CANHT,]$DBH
lowaboveht <- piru[piru$ELEV=="L" & piru$HT>=piru$CANHT,]$HT
lowabovedbh <- piru[piru$ELEV=="L" & piru$HT>=piru$CANHT,]$DBH

lowbelow.mod <- nls(lowbelowht~a*lowbelowdbh^b, start=list(a=1,b=1))
lowabove.mod <- nls(lowaboveht~a*lowabovedbh^b, start=list(a=1,b=1))

midbelowht <- piru[piru$ELEV=="M" & piru$HT<piru$CANHT,]$HT
midbelowdbh <- piru[piru$ELEV=="M" & piru$HT<piru$CANHT,]$DBH
midaboveht <- piru[piru$ELEV=="M" & piru$HT>=piru$CANHT,]$HT
midabovedbh <- piru[piru$ELEV=="M" & piru$HT>=piru$CANHT,]$DBH

midbelow.mod <- nls(midbelowht~a*midbelowdbh^b, start=list(a=1,b=1))
midabove.mod <- nls(midaboveht~a*midabovedbh^b, start=list(a=1,b=1))

highbelowht <- piru[piru$ELEV=="H" & piru$HT<piru$CANHT,]$HT
highbelowdbh <- piru[piru$ELEV=="H" & piru$HT<piru$CANHT,]$DBH
highaboveht <- piru[piru$ELEV=="H" & piru$HT>=piru$CANHT,]$HT
highabovedbh <- piru[piru$ELEV=="H" & piru$HT>=piru$CANHT,]$DBH

highbelow.mod <- nls(highbelowht~a*highbelowdbh^b, start=list(a=1,b=1))
highabove.mod <- nls(highaboveht~a*highabovedbh^b, start=list(a=1,b=1))

###########  Use the nonlinear fits to predict heights for pirus ####################################
############################## 2010 #################################################################
pp$HTPRED10 <- rep(NA,nrow(pp)) ### column that will say whether the height was measured or predicted

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]] == "PIRU" & is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(lowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(lowbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(lowabove.mod)[[1]] * pp$DBH10[[i]]^coef(lowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(midbelow.mod)[[1]] * pp$DBH10[[i]]^coef(midbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(midabove.mod)[[1]] * pp$DBH10[[i]]^coef(midabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(highbelow.mod)[[1]] * pp$DBH10[[i]]^coef(highbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(highabove.mod)[[1]] * pp$DBH10[[i]]^coef(highabove.mod)[[2]])
		}
	}
}

plot(pp[pp$SPECIES=="PIRU",]$DBH10,pp[pp$SPECIES=="PIRU",]$HT10)
points(pp[pp$SPECIES=="PIRU"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="PIRU"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

############################## 1998 #################################################################
pp$HTPRED98 <- rep(NA,nrow(pp)) ### column that will say whether the height was measured or predicted

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]] == "PIRU" & is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(lowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(lowbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(lowabove.mod)[[1]] * pp$DBH98[[i]]^coef(lowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(midbelow.mod)[[1]] * pp$DBH98[[i]]^coef(midbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(midabove.mod)[[1]] * pp$DBH98[[i]]^coef(midabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(highbelow.mod)[[1]] * pp$DBH98[[i]]^coef(highbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(highabove.mod)[[1]] * pp$DBH98[[i]]^coef(highabove.mod)[[2]])
		}
	}
}

plot(pp[pp$SPECIES=="PIRU",]$DBH98,pp[pp$SPECIES=="PIRU",]$HT98)
points(pp[pp$SPECIES=="PIRU"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="PIRU"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

############################## 1986 #################################################################
#### NOTE: since trees were not assigned canopy positions in 86, canopy heights from 98 were used
#### to predict canopy dbhs instead
pp$HTPRED86 <- rep(NA,nrow(pp)) ### column that will say whether the height was measured or predicted

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]] == "PIRU" & is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH98[[i]]) pp$HT86[[i]] <- (coef(lowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(lowbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH98[[i]]) pp$HT86[[i]] <- (coef(lowabove.mod)[[1]] * pp$DBH86[[i]]^coef(lowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH86[[i]]<pp$CANDBH98[[i]]) pp$HT86[[i]] <- (coef(midbelow.mod)[[1]] * pp$DBH86[[i]]^coef(midbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH98[[i]]) pp$HT86[[i]] <- (coef(midabove.mod)[[1]] * pp$DBH86[[i]]^coef(midabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH86[[i]]<pp$CANDBH98[[i]]) pp$HT86[[i]] <- (coef(highbelow.mod)[[1]] * pp$DBH86[[i]]^coef(highbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH98[[i]]) pp$HT86[[i]] <- (coef(highabove.mod)[[1]] * pp$DBH86[[i]]^coef(highabove.mod)[[2]])
		}
	}
}

plot(pp[pp$SPECIES=="PIRU",]$DBH86,pp[pp$SPECIES=="PIRU",]$HT86)
points(pp[pp$SPECIES=="PIRU"&pp$HTPRED86=="1",]$DBH86,pp[pp$SPECIES=="PIRU"&pp$HTPRED86=="1",]$HT86,col="red",lwd=3)


################# summary plot ###################
par(mfrow=c(1,3))
plot(pp[pp$SPECIES=="PIRU",]$DBH86,pp[pp$SPECIES=="PIRU",]$HT86,xlab="DBH86",ylab="HT86",main="1986 pp HT vs DBH with NONLINEAR Predicted Heights")
points(pp[pp$HTPRED86==1,]$DBH86,pp[pp$HTPRED86==1,]$HT86,col="red",lwd=3)
legend(25,5,lwd=3,pch=c(1,1),col=c("red","black"),legend=list("predicted","measured"))

plot(pp[pp$SPECIES=="PIRU",]$DBH98,pp[pp$SPECIES=="PIRU",]$HT98,xlab="DBH98",ylab="HT98",main="1998 pp HT vs DBH with NONLINEAR Predicted Heights")
points(pp[pp$HTPRED98==1,]$DBH98,pp[pp$HTPRED98==1,]$HT98,col="red",lwd=3)
legend(25,5,lwd=3,pch=c(1,1),col=c("red","black"),legend=list("predicted","measured"))

plot(pp[pp$SPECIES=="PIRU",]$DBH10,pp[pp$SPECIES=="PIRU",]$HT10,xlab="DBH10",ylab="HT10",main="2010 pp HT vs DBH with NONLINEAR Predicted Heights")
points(pp[pp$HTPRED10==1,]$DBH10,pp[pp$HTPRED10==1,]$HT10,col="red",lwd=3)
legend(25,5,lwd=3,pch=c(1,1),col=c("red","black"),legend=list("predicted","measured"))

write.csv(pp,"C:/Documents and Settings/Noah/Desktop/ppnew.csv")






