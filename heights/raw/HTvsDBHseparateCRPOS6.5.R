### 6/5 ht vs. dbh with differently marked points for codominant and dominant trees
### different fits for both groups, codom+dom and int+overtop

pp <- read.csv("C:/Users/Noah/Desktop/canopy.csv",header=T,sep=",")
abba <- subset(pp, SPECIES=="ABBA")

##################################### 1998 data ##############################################
par(mfrow=c(1,3))
### Plot of ABBA at low elevation
plot(abba[abba$ELEV=="L",]$DBH98,abba[abba$ELEV=="L",]$HT98,pch="",xlab="DBH",ylab="Height",
	main="Abba at Low Elevation 1998, Height vs. DBH for Overtopped+Intermediate
	and Codominant+Dominant Trees")
points(x=abba[abba$ELEV=="L"&abba$CRPOS98=="o" | abba$ELEV=="L"&abba$CRPOS98=="i",]$DBH98,
	y=abba[abba$ELEV=="L"&abba$CRPOS98=="o" | abba$ELEV=="L"&abba$CRPOS98=="i",]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="L"&abba$CRPOS98=="c" | abba$ELEV=="L"&abba$CRPOS98=="d",]$DBH98,
	y=abba[abba$ELEV=="L"&abba$CRPOS98=="c" | abba$ELEV=="L"&abba$CRPOS98=="d",]$HT98, pch = 3, col = "red")
abline(lowlowcr98.fit,col="green",lwd = 2)
abline(lowhighcr98.fit,col="red", lwd = 2)
### Add canopy fits to compare
abline(lowbelow98.fit,col="purple", lwd = 2, lty = 2)
abline(lowabove98.fit,col="yellow", lwd = 2, lty = 2)

legend(20,5,pch = c(2,3,95,95), col=c("green","red","purple","yellow"),
	legend = list("Overtop+Int","Codom+Dom","Below Canopy","Above Canopy"))

### Plot of ABBA at mid elevation
plot(abba[abba$ELEV=="M",]$DBH98,abba[abba$ELEV=="M",]$HT98,pch="",xlab="DBH",ylab="Height",
	main="Abba at Mid Elevation 1998, Height vs. DBH for Overtopped+Intermediate
	and Codominant+Dominant Trees")

points(x=abba[abba$ELEV=="M"&abba$CRPOS98=="o" | abba$ELEV=="M"&abba$CRPOS98=="i",]$DBH98,
	y=abba[abba$ELEV=="M"&abba$CRPOS98=="o" | abba$ELEV=="M"&abba$CRPOS98=="i",]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="M"&abba$CRPOS98=="c" | abba$ELEV=="M"&abba$CRPOS98=="d",]$DBH98,
	y=abba[abba$ELEV=="M"&abba$CRPOS98=="c" | abba$ELEV=="M"&abba$CRPOS98=="d",]$HT98, pch = 3, col = "red")
abline(midlowcr98.fit,col="green",lwd = 2)
abline(midhighcr98.fit,col="red", lwd = 2)
### Add canopy fits to compare
abline(midbelow98.fit,col="purple", lwd = 2, lty = 2)
abline(midabove98.fit,col="yellow", lwd = 2, lty = 2)

legend(20,5,pch = c(2,3,95,95), col=c("green","red","purple","yellow"),
	legend = list("Overtop+Int","Codom+Dom","Below Canopy","Above Canopy"))

### Plot of ABBA at high elevation
plot(abba[abba$ELEV=="H",]$DBH98,abba[abba$ELEV=="H",]$HT98,pch="",xlab="DBH",ylab="Height",
	main="Abba at Mid Elevation 1998, Height vs. DBH for Overtopped+Intermediate
	and Codominant+Dominant Trees")

points(x=abba[abba$ELEV=="H"&abba$CRPOS98=="o" | abba$ELEV=="H"&abba$CRPOS98=="i",]$DBH98,
	y=abba[abba$ELEV=="H"&abba$CRPOS98=="o" | abba$ELEV=="H"&abba$CRPOS98=="i",]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="H"&abba$CRPOS98=="c" | abba$ELEV=="H"&abba$CRPOS98=="d",]$DBH98,
	y=abba[abba$ELEV=="H"&abba$CRPOS98=="c" | abba$ELEV=="H"&abba$CRPOS98=="d",]$HT98, pch = 3, col = "red")
abline(highlowcr98.fit,col="green",lwd = 2)
abline(highhighcr98.fit,col="red", lwd = 2)
### Add fits from 2010 to compare
abline(highbelow98.fit,col="purple", lwd = 2,lty=2)
abline(highabove98.fit,col="yellow", lwd = 2,lty=2)

legend(20,5,pch = c(2,3,95,95), col=c("green","red","purple","yellow"),
	legend = list("Overtop+Int","Codom+Dom","Below Canopy","Above Canopy"))

########## linear fits ############################################
lowbelow98.fit <- lm(abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="L"&abba$HT98<abba$CANHT98,]$DBH98)
lowabove98.fit <- lm(abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$DBH98)
lowlowcr98.fit <- lm(abba[abba$ELEV=="L"&abba$CRPOS98=="o" | abba$ELEV=="L"&abba$CRPOS98=="i",]$HT98~
	abba[abba$ELEV=="L"&abba$CRPOS98=="o" | abba$ELEV=="L"&abba$CRPOS98=="i",]$DBH98)
lowhighcr98.fit <- lm(abba[abba$ELEV=="L"&abba$CRPOS98=="c" | abba$ELEV=="L"&abba$CRPOS98=="d",]$HT98~
	abba[abba$ELEV=="L"&abba$CRPOS98=="c" | abba$ELEV=="L"&abba$CRPOS98=="d",]$DBH98)

midbelow98.fit <- lm(abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="M"&abba$HT98<abba$CANHT98,]$DBH98)
midabove98.fit <- lm(abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="M"&abba$HT98>=abba$CANHT98,]$DBH98)
midlowcr98.fit <- lm(abba[abba$ELEV=="M"&abba$CRPOS98=="o" | abba$ELEV=="M"&abba$CRPOS98=="i",]$HT98~
	abba[abba$ELEV=="M"&abba$CRPOS98=="o" | abba$ELEV=="M"&abba$CRPOS98=="i",]$DBH98)
midhighcr98.fit <- lm(abba[abba$ELEV=="M"&abba$CRPOS98=="c" | abba$ELEV=="M"&abba$CRPOS98=="d",]$HT98~
	abba[abba$ELEV=="M"&abba$CRPOS98=="c" | abba$ELEV=="M"&abba$CRPOS98=="d",]$DBH98)

highbelow98.fit <- lm(abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$HT98~
	abba[abba$ELEV=="H"&abba$HT98<abba$CANHT98,]$DBH98)
highabove98.fit <- lm(abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$HT98~
	abba[abba$ELEV=="H"&abba$HT98>=abba$CANHT98,]$DBH98)
highlowcr98.fit <- lm(abba[abba$ELEV=="H"&abba$CRPOS98=="o" | abba$ELEV=="H"&abba$CRPOS98=="i",]$HT98~
	abba[abba$ELEV=="H"&abba$CRPOS98=="o" | abba$ELEV=="H"&abba$CRPOS98=="i",]$DBH98)
highhighcr98.fit <- lm(abba[abba$ELEV=="H"&abba$CRPOS98=="c" | abba$ELEV=="H"&abba$CRPOS98=="d",]$HT98~
	abba[abba$ELEV=="H"&abba$CRPOS98=="c" | abba$ELEV=="H"&abba$CRPOS98=="d",]$DBH98)


