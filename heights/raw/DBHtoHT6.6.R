### 6.6 Estimating heights for ABBA from DBH using separate fits for below and
### above canopy,  start with linear fits from 98 data from HtvsDBHseparateCRPOS6.5.R
### script.
### Procedure:: canopy heights are estimated on a plot by plot basis from the mean of
### of the codominant trees in each plot, mean dbh of canopy trees in each plot will
### determine if tree height will be fit using the above or below canopy fit for each
### elevation: H,M,L
### NOTE :: only 48 data entries have a DBH98 and no HT98

pp <- read.csv("C:/Documents and Settings/Noah/Desktop/ppnew.csv",header=T,sep=",")
abba <- subset(pp, SPECIES=="ABBA")

########################## linear fits above and below canopy ######################
lowbelow98.fit <- lm(abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$DBH98)
lowabove98.fit <- lm(abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$DBH98)
lowbelow10.fit <- lm(abba[abba$ELEV=="L"&abba$HT10<abba$CANHT10,]$HT10~
	abba[abba$ELEV=="L"&abba$HT10<abba$CANHT10,]$DBH10)
lowabove10.fit <- lm(abba[abba$ELEV=="L"&abba$HT10>=abba$CANHT10,]$HT10~
	abba[abba$ELEV=="L"&abba$HT10>=abba$CANHT10,]$DBH10)

midbelow98.fit <- lm(abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$DBH98)
midabove98.fit <- lm(abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$DBH98)
midbelow10.fit <- lm(abba[abba$ELEV=="M"&abba$HT10<abba$CANHT10,]$HT10~
	abba[abba$ELEV=="M"&abba$HT10<abba$CANHT10,]$DBH10)
midabove10.fit <- lm(abba[abba$ELEV=="M"&abba$HT10>=abba$CANHT10,]$HT10~
	abba[abba$ELEV=="M"&abba$HT10>=abba$CANHT10,]$DBH10)

highbelow98.fit <- lm(abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$DBH98)
highabove98.fit <- lm(abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$DBH98)
highbelow10.fit <- lm(abba[abba$ELEV=="H"&abba$HT10<abba$CANHT10,]$HT10~
	abba[abba$ELEV=="H"&abba$HT10<abba$CANHT10,]$DBH10)
highabove10.fit <- lm(abba[abba$ELEV=="H"&abba$HT10>=abba$CANHT10,]$HT10~
	abba[abba$ELEV=="H"&abba$HT10>=abba$CANHT10,]$DBH10)

####################################################################################
abba$HTPRED98 <- rep(NA,nrow(abba)) ### column that will say whether the height was measured or predicted

for(i in 1:nrow(abba)) {
	if(is.na(abba$HT98[[i]]) & !is.na(abba$DBH98[[i]])) {
		abba$HTPRED98[[i]] <- 1
		if(abba$ELEV[[i]] == "L") {
			if(abba$DBH98[[i]]<abba$CANDBH98[[i]]) abba$HT98[[i]] <- (coef(lowbelow98.fit)[[1]] + abba$DBH98[[i]]*coef(lowbelow98.fit)[[2]])
			if(abba$DBH98[[i]]>=abba$CANDBH98[[i]]) abba$HT98[[i]] <- (coef(lowabove98.fit)[[1]] + abba$DBH98[[i]]*coef(lowabove98.fit)[[2]])
		}
		if(abba$ELEV[[i]] == "M") {
			if(abba$DBH98[[i]]<abba$CANDBH98[[i]]) abba$HT98[[i]] <- (coef(midbelow98.fit)[[1]] + abba$DBH98[[i]]*coef(midbelow98.fit)[[2]])
			if(abba$DBH98[[i]]>=abba$CANDBH98[[i]]) abba$HT98[[i]] <- (coef(midabove98.fit)[[1]] + abba$DBH98[[i]]*coef(midabove98.fit)[[2]])
		}
		if(abba$ELEV[[i]] == "H") {
			if(abba$DBH98[[i]]<abba$CANDBH98[[i]]) abba$HT98[[i]] <- (coef(highbelow98.fit)[[1]] + abba$DBH98[[i]]*coef(highbelow98.fit)[[2]])
			if(abba$DBH98[[i]]>=abba$CANDBH98[[i]]) abba$HT98[[i]] <- (coef(highabove98.fit)[[1]] + abba$DBH98[[i]]*coef(highabove98.fit)[[2]])
		}
	}
}

plot(abba$DBH98,abba$HT98)
points(abba[abba$HTPRED98=="1",]$DBH98,abba[abba$HTPRED98=="1",]$HT98,col="red",lwd=3)

######################################## 2010 ############################################
abba$HTPRED10 <- rep(NA,nrow(abba)) ### column that will say whether the height was measured or predicted

for(i in 1:nrow(abba)) {
	if(is.na(abba$HT10[[i]]) & !is.na(abba$DBH10[[i]])) {
		abba$HTPRED10[[i]] <- 1
		if(abba$ELEV[[i]] == "L") {
			if(abba$DBH10[[i]]<abba$CANDBH10[[i]]) abba$HT10[[i]] <- (coef(lowbelow10.fit)[[1]] + abba$DBH10[[i]]*coef(lowbelow10.fit)[[2]])
			if(abba$DBH10[[i]]>=abba$CANDBH10[[i]]) abba$HT10[[i]] <- (coef(lowabove10.fit)[[1]] + abba$DBH10[[i]]*coef(lowabove10.fit)[[2]])
		}
		if(abba$ELEV[[i]] == "M") {
			if(abba$DBH10[[i]]<abba$CANDBH10[[i]]) abba$HT10[[i]] <- (coef(midbelow10.fit)[[1]] + abba$DBH10[[i]]*coef(midbelow10.fit)[[2]])
			if(abba$DBH10[[i]]>=abba$CANDBH10[[i]]) abba$HT10[[i]] <- (coef(midabove10.fit)[[1]] + abba$DBH10[[i]]*coef(midabove10.fit)[[2]])
		}
		if(abba$ELEV[[i]] == "H") {
			if(abba$DBH10[[i]]<abba$CANDBH10[[i]]) abba$HT10[[i]] <- (coef(highbelow10.fit)[[1]] + abba$DBH10[[i]]*coef(highbelow10.fit)[[2]])
			if(abba$DBH10[[i]]>=abba$CANDBH10[[i]]) abba$HT10[[i]] <- (coef(highabove10.fit)[[1]] + abba$DBH10[[i]]*coef(highabove10.fit)[[2]])
		}
	}
}

plot(abba$DBH10,abba$HT10)
points(abba[abba$HTPRED10=="1",]$DBH10,abba[abba$HTPRED10=="1",]$HT10,col="red",lwd=3)


################# summary plot ###################
par(mfrow=c(1,2))
plot(abba$DBH98,abba$HT98,xlab="DBH98",ylab="HT98",main="1998 ABBA HT vs DBH with LINEARLY Predicted Heights,
	48 Predicted Heights")
points(abba[abba$HTPRED98=="1",]$DBH98,abba[abba$HTPRED98=="1",]$HT98,col="red",lwd=3)
legend(25,5,lwd=3,pch=c(1,1),col=c("red","black"),legend=list("predicted","measured"))

plot(abba$DBH10,abba$HT10,xlab="DBH10",ylab="HT10",main="2010 ABBA HT vs DBH with LINEARLY Predicted Heights,
	5221 Predicted Heights")
points(abba[abba$HTPRED10=="1",]$DBH10,abba[abba$HTPRED10=="1",]$HT10,col="red",lwd=3)
legend(25,5,lwd=3,pch=c(1,1),col=c("red","black"),legend=list("predicted","measured"))


######################################## Nonlinear Fits ######################################################
htlowbelow98 <- abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$HT98
dbhlowbelow98 <- abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$DBH98
htlowabove98 <- abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$HT98
dbhlowabove98 <- abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$DBH98
htlowbelow10 <- abba[abba$ELEV=="L"&abba$HT10<abba$CANHT10,]$HT10
dbhlowbelow10 <- abba[abba$ELEV=="L"&abba$HT10<abba$CANHT10,]$DBH10
htlowabove10 <- abba[abba$ELEV=="L"&abba$HT10>=abba$CANHT10,]$HT10
dbhlowabove10 <- abba[abba$ELEV=="L"&abba$HT10>=abba$CANHT10,]$DBH10
### Mid ###
htmidbelow98 <- abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$HT98
dbhmidbelow98 <- abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$DBH98
htmidabove98 <- abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$HT98
dbhmidabove98 <- abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$DBH98
htmidbelow10 <- abba[abba$ELEV=="M"&abba$HT10<abba$CANHT10,]$HT10
dbhmidbelow10 <- abba[abba$ELEV=="M"&abba$HT10<abba$CANHT10,]$DBH10
htmidabove10 <- abba[abba$ELEV=="M"&abba$HT10>=abba$CANHT10,]$HT10
dbhmidabove10 <- abba[abba$ELEV=="M"&abba$HT10>=abba$CANHT10,]$DBH10
### High ###
hthighbelow98 <- abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$HT98
dbhhighbelow98 <- abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$DBH98
hthighabove98 <- abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$HT98
dbhhighabove98 <- abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$DBH98
hthighbelow10 <- abba[abba$ELEV=="H"&abba$HT10<abba$CANHT10,]$HT10
dbhhighbelow10 <- abba[abba$ELEV=="H"&abba$HT10<abba$CANHT10,]$DBH10
hthighabove10 <- abba[abba$ELEV=="H"&abba$HT10>=abba$CANHT10,]$HT10
dbhhighabove10 <- abba[abba$ELEV=="H"&abba$HT10>=abba$CANHT10,]$DBH10

lowbelow98.mod <- nls(htlowbelow98~a*dbhlowbelow98^b,start=list(a=1,b=2))
lowabove98.mod <- nls(htlowabove98~a*dbhlowabove98^b,start=list(a=1,b=2))
lowbelow10.mod <- nls(htlowbelow10~a*dbhlowbelow10^b,start=list(a=1,b=2))
lowabove10.mod <- nls(htlowabove10~a*dbhlowabove10^b,start=list(a=1,b=2))

midbelow98.mod <- nls(htmidbelow98~a*dbhmidbelow98^b,start=list(a=1,b=2))
midabove98.mod <- nls(htmidabove98~a*dbhmidabove98^b,start=list(a=1,b=2))
midbelow10.mod <- nls(htmidbelow10~a*dbhmidbelow10^b,start=list(a=1,b=2))
midabove10.mod <- nls(htmidabove10~a*dbhmidabove10^b,start=list(a=1,b=2))

highbelow98.mod <- nls(hthighbelow98~a*dbhhighbelow98^b,start=list(a=1,b=2))
highabove98.mod <- nls(hthighabove98~a*dbhhighabove98^b,start=list(a=1,b=2))
highbelow10.mod <- nls(hthighbelow10~a*dbhhighbelow10^b,start=list(a=1,b=2))
highabove10.mod <- nls(hthighabove10~a*dbhhighabove10^b,start=list(a=1,b=2))
####################################################################################
######################################## 1998 ############################################
###abba$HTPRED98 <- rep(NA,nrow(abba)) ### column that will say whether the height was measured or predicted

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(lowbelow98.mod)[[1]] * pp$DBH98[[i]]^coef(lowbelow98.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(lowabove98.mod)[[1]] * pp$DBH98[[i]]^coef(lowabove98.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(midbelow98.mod)[[1]] * pp$DBH98[[i]]^coef(midbelow98.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(midabove98.mod)[[1]] * pp$DBH98[[i]]^coef(midabove98.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(highbelow98.mod)[[1]] * pp$DBH98[[i]]^coef(highbelow98.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(highabove98.mod)[[1]] * pp$DBH98[[i]]^coef(highabove98.mod)[[2]])
		}
	}
}

plot(pp[pp$SPECIES=="ABBA",]$DBH98,pp[pp$SPECIES=="ABBA",]$HT98)
points(pp[pp$SPECIES=="ABBA"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="ABBA"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

######################################## 2010 ############################################
###abba$HTPRED10 <- rep(NA,nrow(abba)) ### column that will say whether the height was measured or predicted

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(lowbelow10.mod)[[1]] * pp$DBH10[[i]]^coef(lowbelow10.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(lowabove10.mod)[[1]] * pp$DBH10[[i]]^coef(lowabove10.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(midbelow10.mod)[[1]] * pp$DBH10[[i]]^coef(midbelow10.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(midabove10.mod)[[1]] * pp$DBH10[[i]]^coef(midabove10.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(highbelow10.mod)[[1]] * pp$DBH10[[i]]^coef(highbelow10.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(highabove10.mod)[[1]] * pp$DBH10[[i]]^coef(highabove10.mod)[[2]])
		}
	}
}

plot(pp[pp$SPECIES=="ABBA",]$DBH10,pp[pp$SPECIES=="ABBA",]$HT10)
points(pp[pp$SPECIES=="ABBA"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="ABBA"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

################# summary plot ###################
par(mfrow=c(1,2))
plot(abba$DBH98,abba$HT98,xlab="DBH98",ylab="HT98",main="1998 ABBA HT vs DBH with NONLINEAR Predicted Heights,
	48 Predicted Heights")
points(abba[abba$HTPRED98==1,]$DBH98,abba[abba$HTPRED98==1,]$HT98,col="red",lwd=3)
legend(25,5,lwd=3,pch=c(1,1),col=c("red","black"),legend=list("predicted","measured"))

plot(abba$DBH10,abba$HT10,xlab="DBH10",ylab="HT10",main="2010 ABBA HT vs DBH with NONLINEAR Predicted Heights,
	5221 Predicted Heights")
points(abba[abba$HTPRED10==1,]$DBH10,abba[abba$HTPRED10==1,]$HT10,col="red",lwd=3)
legend(25,5,lwd=3,pch=c(1,1),col=c("red","black"),legend=list("predicted","measured"))




write.csv(pp,"C:/Documents and Settings/Noah/Desktop/ppnew.csv")


###### 1986 ######################
### NOTE: there are no canopy positions for trees in 86

nrow(pp[pp$SPECIES=="ABBA"&!is.na(pp$HT86)&!is.na(pp$DBH86)&pp$ELEV=="H",])
plot(pp[pp$SPECIES=="ABBA"&pp$ELEV=="H",]$DBH86,pp[pp$SPECIES=="ABBA"&pp$ELEV=="H",]$HT86)





table(pp$CANHT98,pp$PLOT)








