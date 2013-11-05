### 5/17
### create HT vs DBH scatters with differently marked points for trees 
### with heights at or above canopy level
pp <- read.csv("C:/Users/Noah/Desktop/canopy.csv",header=T,sep=",")
abba <- subset(pp, SPECIES=="ABBA")

##################################### 2010 data ##############################################3
par(mfrow=c(1,3))
### Plot of ABBA at low elevation
plot(abba[abba$ELEV=="L",]$DBH10,abba[abba$ELEV=="L",]$HT10,pch="",xlab="DBH",ylab="Height",
	main="Abba at Low Elevation 2010, Height vs. DBH for Trees
	Above and Below Canopy Level")

points(x=abba[abba$ELEV=="L"&abba$HT10<abba$CANHT10,]$DBH10,
	y=abba[abba$ELEV=="L"&abba$HT10<abba$CANHT10,]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="L"&abba$HT10>=abba$CANHT10,]$DBH10,
	y=abba[abba$ELEV=="L"&abba$HT10>=abba$CANHT10,]$HT10, pch = 3, col = "red")
abline(lowbelow.fit,col="green")
abline(lowabove.fit,col="red")
legend(20,5,pch = c(2,3), col=c("green","red"),
	legend = list("Below Canopy","At or Above Canopy"))

### Plot of ABBA at mid elevation
plot(abba[abba$ELEV=="M",]$DBH10,abba[abba$ELEV=="M",]$HT10,pch="",xlab="DBH",ylab="Height",
	main="Abba at Mid Elevation 2010, Height vs. DBH for Trees
	Above and Below Canopy Level")

points(x=abba[abba$ELEV=="M"&abba$HT10<abba$CANHT10,]$DBH10,
	y=abba[abba$ELEV=="M"&abba$HT10<abba$CANHT10,]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="M"&abba$HT10>=abba$CANHT10,]$DBH10,
	y=abba[abba$ELEV=="M"&abba$HT10>=abba$CANHT10,]$HT10, pch = 3, col = "red")
abline(midbelow.fit,col="green")
abline(midabove.fit,col="red")
legend(20,5,pch = c(2,3), col=c("green","red"),
	legend = list("Below Canopy","At or Above Canopy"))

### Plot of ABBA at high elevation
plot(abba[abba$ELEV=="H",]$DBH10,abba[abba$ELEV=="H",]$HT10,pch="",xlab="DBH",ylab="Height",
	main="Abba at High Elevation 2010, Height vs. DBH for Trees
	Above and Below Canopy Level")

points(x=abba[abba$ELEV=="H"&abba$HT10<abba$CANHT10,]$DBH10,
	y=abba[abba$ELEV=="H"&abba$HT10<abba$CANHT10,]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="H"&abba$HT10>=abba$CANHT10,]$DBH10,
	y=abba[abba$ELEV=="H"&abba$HT10>=abba$CANHT10,]$HT10, pch = 3, col = "red")
abline(highbelow.fit,col="green")
abline(highabove.fit,col="red")
legend(20,5,pch = c(2,3), col=c("green","red"),
	legend = list("Below Canopy","At or Above Canopy"))

########## linear fits ############################################
lowbelow.fit <- lm(abba[abba$ELEV=="L"&abba$HT10<abba$CANHT10,]$HT10~
	abba[abba$ELEV=="L"&abba$HT10<abba$CANHT10,]$DBH10)
lowabove.fit <- lm(abba[abba$ELEV=="L"&abba$HT10>=abba$CANHT10,]$HT10~
	abba[abba$ELEV=="L"&abba$HT10>=abba$CANHT10,]$DBH10)
midbelow.fit <- lm(abba[abba$ELEV=="M"&abba$HT10<abba$CANHT10,]$HT10~
	abba[abba$ELEV=="M"&abba$HT10<abba$CANHT10,]$DBH10)
midabove.fit <- lm(abba[abba$ELEV=="M"&abba$HT10>=abba$CANHT10,]$HT10~
	abba[abba$ELEV=="M"&abba$HT10>=abba$CANHT10,]$DBH10)
highbelow.fit <- lm(abba[abba$ELEV=="H"&abba$HT10<abba$CANHT10,]$HT10~
	abba[abba$ELEV=="H"&abba$HT10<abba$CANHT10,]$DBH10)
highabove.fit <- lm(abba[abba$ELEV=="H"&abba$HT10>=abba$CANHT10,]$HT10~
	abba[abba$ELEV=="H"&abba$HT10>=abba$CANHT10,]$DBH10)


##################################### 1998 data ##############################################3
par(mfrow=c(1,3))
### Plot of ABBA at low elevation
plot(abba[abba$ELEV=="L",]$DBH98,abba[abba$ELEV=="L",]$HT98,pch="",xlab="DBH",ylab="Height",
	main="Abba at Low Elevation 1998, Height vs. DBH for Trees
	Above and Below Canopy Level")

points(x=abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$DBH98,
	y=abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$DBH98,
	y=abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$HT98, pch = 3, col = "red")
abline(lowbelow98.fit,col="green",lwd = 2)
abline(lowabove98.fit,col="red", lwd = 2)
### Add fits from 2010 to compare
abline(lowbelow.fit,col="purple", lwd = 2)
abline(lowabove.fit,col="yellow", lwd = 2)

legend(20,5,pch = c(2,3,95,95), col=c("green","red","purple","yellow"),
	legend = list("2010 Below Canopy","2010 At or Above Canopy","1998 Below","1998 At or Above"))

### Plot of ABBA at mid elevation
plot(abba[abba$ELEV=="M",]$DBH98,abba[abba$ELEV=="M",]$HT98,pch="",xlab="DBH",ylab="Height",
	main="Abba at Mid Elevation 1998, Height vs. DBH for Trees
	Above and Below Canopy Level")

points(x=abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$DBH98,
	y=abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$DBH98,
	y=abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$HT98, pch = 3, col = "red")
abline(midbelow98.fit,col="green", lwd = 2)
abline(midabove98.fit,col="red", lwd = 2)
### Add fits from 2010 to compare
abline(midbelow.fit,col="purple", lwd = 2)
abline(midabove.fit,col="yellow", lwd = 2)

legend(20,5,pch = c(2,3,95,95), col=c("green","red","purple","yellow"),
	legend = list("2010 Below Canopy","2010 At or Above Canopy","1998 Below","1998 At or Above"))

### Plot of ABBA at high elevation
plot(abba[abba$ELEV=="H",]$DBH98,abba[abba$ELEV=="H",]$HT98,pch="",xlab="DBH",ylab="Height",
	main="Abba at High Elevation 1998, Height vs. DBH for Trees
	Above and Below Canopy Level")

points(x=abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$DBH98,
	y=abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$DBH98,
	y=abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$HT98, pch = 3, col = "red")
abline(highbelow98.fit,col="green", lwd = 2)
abline(highabove98.fit,col="red", lwd = 2)
### Add fits from 2010 to compare
abline(highbelow.fit,col="purple", lwd = 2)
abline(highabove.fit,col="yellow", lwd = 2)

legend(20,5,pch = c(2,3,95,95), col=c("green","red","purple","yellow"),
	legend = list("2010 Below Canopy","2010 At or Above Canopy","1998 Below","1998 At or Above"))

########## linear fits ############################################
lowbelow98.fit <- lm(abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$DBH98)
lowabove98.fit <- lm(abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$DBH98)
midbelow98.fit <- lm(abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$DBH98)
midabove98.fit <- lm(abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$DBH98)
highbelow98.fit <- lm(abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$DBH98)
highabove98.fit <- lm(abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$DBH98)










