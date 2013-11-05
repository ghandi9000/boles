### BECO heights 6/8
pp <- read.csv("C:/Documents and Settings/Noah/My Documents/Dropbox/Noah/ppnew.csv",header=T)

### sample sizes of BECO that have both dbh and ht for the different years

a <- nrow(pp[pp$SPECIES == "BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]) ### 459 from 1986
b <- nrow(pp[pp$SPECIES == "BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98),]) ### 566 from 1998
c <- nrow(pp[pp$SPECIES == "BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10),]) ### 161 from 2010
d <- nrow(pp[pp$SPECIES == "BECO" & !is.na(pp$HT11) & !is.na(pp$DBH11),]) ### 0 from 2011

### sample sizes from the different years that were not previously sampled

nrow(pp[pp$SPECIES == "BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]) ### 339 from 1998 that did not have heights in 1986
nrow(pp[pp$SPECIES == "BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]) ### 18 from 2010 that did not have heights in 1986 or 1998

### total usable sample size of 816

beco <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) | ### 98 trees
	pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT98
year <- rep(86,length(ht86))
beco <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(beco) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$DBH98
ht98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$HT98
elev98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$ELEV
plots98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$PLOT
canht98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

beco <- rbind(beco,data98)
beco <- rbind(beco,data10)

### linear fits above and below canopy ##############################
lowbelow.fit <- lm(beco[beco$ELEV=="L" & beco$HT<beco$CANHT,]$HT~beco[beco$ELEV=="L" & beco$HT<beco$CANHT,]$DBH)
lowabove.fit <- lm(beco[beco$ELEV=="L" & beco$HT>=beco$CANHT,]$HT~beco[beco$ELEV=="L" & beco$HT>=beco$CANHT,]$DBH)

midbelow.fit <- lm(beco[beco$ELEV=="M" & beco$HT<beco$CANHT,]$HT~beco[beco$ELEV=="M" & beco$HT<beco$CANHT,]$DBH)
midabove.fit <- lm(beco[beco$ELEV=="M" & beco$HT>=beco$CANHT,]$HT~beco[beco$ELEV=="M" & beco$HT>=beco$CANHT,]$DBH)

highbelow.fit <- lm(beco[beco$ELEV=="H" & beco$HT<beco$CANHT,]$HT~beco[beco$ELEV=="H" & beco$HT<beco$CANHT,]$DBH)
highabove.fit <- lm(beco[beco$ELEV=="H" & beco$HT>=beco$CANHT,]$HT~beco[beco$ELEV=="H" & beco$HT>=beco$CANHT,]$DBH)

############# nonlinear fits above and below canopy ##########################
lowbelowht <- beco[beco$ELEV=="L" & beco$HT<beco$CANHT,]$HT
lowbelowdbh <- beco[beco$ELEV=="L" & beco$HT<beco$CANHT,]$DBH
lowaboveht <- beco[beco$ELEV=="L" & beco$HT>=beco$CANHT,]$HT
lowabovedbh <- beco[beco$ELEV=="L" & beco$HT>=beco$CANHT,]$DBH

lowbelow.mod <- nls(lowbelowht~a*lowbelowdbh^b, start=list(a=1,b=1))
lowabove.mod <- nls(lowaboveht~a*lowabovedbh^b, start=list(a=1,b=1))

midbelowht <- beco[beco$ELEV=="M" & beco$HT<beco$CANHT,]$HT
midbelowdbh <- beco[beco$ELEV=="M" & beco$HT<beco$CANHT,]$DBH
midaboveht <- beco[beco$ELEV=="M" & beco$HT>=beco$CANHT,]$HT
midabovedbh <- beco[beco$ELEV=="M" & beco$HT>=beco$CANHT,]$DBH

midbelow.mod <- nls(midbelowht~a*midbelowdbh^b, start=list(a=1,b=1))
midabove.mod <- nls(midaboveht~a*midabovedbh^b, start=list(a=1,b=1))

highbelowht <- beco[beco$ELEV=="H" & beco$HT<beco$CANHT,]$HT
highbelowdbh <- beco[beco$ELEV=="H" & beco$HT<beco$CANHT,]$DBH
highaboveht <- beco[beco$ELEV=="H" & beco$HT>=beco$CANHT,]$HT
highabovedbh <- beco[beco$ELEV=="H" & beco$HT>=beco$CANHT,]$DBH

highbelow.mod <- nls(highbelowht~a*highbelowdbh^b, start=list(a=1,b=1))
highabove.mod <- nls(highaboveht~a*highabovedbh^b, start=list(a=1,b=1))

#### summary plots
par(mfrow=c(1,3))
plot(beco[beco$ELEV=="L",]$DBH,beco[beco$ELEV=="L",]$HT,pch="")
points(beco[beco$ELEV=="L"&beco$YEAR==86,]$DBH,beco[beco$ELEV=="L"&beco$YEAR==86,]$HT,col="blue")
points(beco[beco$ELEV=="L"&beco$YEAR==98,]$DBH,beco[beco$ELEV=="L"&beco$YEAR==98,]$HT,col="red")
points(beco[beco$ELEV=="L"&beco$YEAR==10,]$DBH,beco[beco$ELEV=="L"&beco$YEAR==10,]$HT,col="green")
abline(lowbelow.fit)
abline(lowabove.fit)
curve(coef(becolowbelow.mod)[[1]]*x^coef(becolowbelow.mod)[[2]],col="blue",add=T)
curve(coef(becolowabove.mod)[[1]]*x^coef(becolowabove.mod)[[2]],col="green",add=T)

plot(beco[beco$ELEV=="M",]$DBH,beco[beco$ELEV=="M",]$HT,pch="")
points(beco[beco$ELEV=="M"&beco$YEAR==86,]$DBH,beco[beco$ELEV=="M"&beco$YEAR==86,]$HT,col="blue")
points(beco[beco$ELEV=="M"&beco$YEAR==98,]$DBH,beco[beco$ELEV=="M"&beco$YEAR==98,]$HT,col="red")
points(beco[beco$ELEV=="M"&beco$YEAR==10,]$DBH,beco[beco$ELEV=="M"&beco$YEAR==10,]$HT,col="green")
abline(midbelow.fit)
abline(midabove.fit)
curve(coef(becomidbelow.mod)[[1]]*x^coef(becomidbelow.mod)[[2]],col="blue",add=T)
curve(coef(becomidabove.mod)[[1]]*x^coef(becomidabove.mod)[[2]],col="green",add=T)

plot(beco[beco$ELEV=="H",]$DBH,beco[beco$ELEV=="H",]$HT,pch="")
points(beco[beco$ELEV=="H"&beco$YEAR==86,]$DBH,beco[beco$ELEV=="H"&beco$YEAR==86,]$HT,col="blue")
points(beco[beco$ELEV=="H"&beco$YEAR==98,]$DBH,beco[beco$ELEV=="H"&beco$YEAR==98,]$HT,col="red")
points(beco[beco$ELEV=="H"&beco$YEAR==10,]$DBH,beco[beco$ELEV=="H"&beco$YEAR==10,]$HT,col="green")
abline(highbelow.fit)
abline(highabove.fit)
curve(coef(becohighbelow.mod)[[1]]*x^coef(becohighbelow.mod)[[2]],col="blue",add=T)
curve(coef(becohighabove.mod)[[1]]*x^coef(becohighabove.mod)[[2]],col="green",add=T)


###########  Use the nonlinear fits to predict heights for becos ####################################
############################## 2010 #################################################################

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]] == "BECO" & is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]])) {
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

plot(pp[pp$SPECIES=="BECO",]$DBH10,pp[pp$SPECIES=="BECO",]$HT10)
points(pp[pp$SPECIES=="BECO"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="BECO"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

############################## 1998 #################################################################

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]] == "BECO" & is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]])) {
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

plot(pp[pp$SPECIES=="BECO",]$DBH98,pp[pp$SPECIES=="BECO",]$HT98)
points(pp[pp$SPECIES=="BECO"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="BECO"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

############################## 1986 #################################################################
#### NOTE: since trees were not assigned canopy positions in 86, canopy heights from 98 were used
#### to predict canopy dbhs instead

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]] == "BECO" & is.na(pp$HT86[[i]]) & !is.na(pp$CANDBH98[[i]]) & !is.na(pp$DBH86[[i]])) {
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

plot(pp[pp$SPECIES=="BECO"&pp$PLOT>3,]$DBH86,pp[pp$SPECIES=="BECO"&pp$PLOT>3,]$HT86)
points(pp[pp$SPECIES=="BECO"&pp$HTPRED86=="1",]$DBH86,pp[pp$SPECIES=="BECO"&pp$HTPRED86=="1",]$HT86,col="red",lwd=3)

table(pp[pp$SPECIES=="BECO"&pp$HTPRED86=="1",]$ELEV)

write.csv(pp,"C:/Documents and Settings/Noah/Desktop/ppnew.csv")





