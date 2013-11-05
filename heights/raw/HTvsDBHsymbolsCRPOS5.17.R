### 5/17 
### create scatterplot ht vs. dbh for 2010 trees with different symbols
### pertaining to different crown positions (o,i,c,d)

### root mean square error to compare models
rmse <- function(obs, pred) sqrt(mean((obs-pred)^2))
### Abba at three elevations 2010
abba<-subset(pp, SPECIES == "ABBA")
windows()
par(mfrow = c(1,3))
plot(abba[abba$ELEV=="L",]$DBH10,abba[abba$ELEV=="L",]$HT10)
low.fit <- lm(abba[abba$ELEV=="L",]$HT98~abba[abba$ELEV=="L",]$DBH10)
abline(low.fit)
plot(abba[abba$ELEV=="M",]$DBH10,abba[abba$ELEV=="M",]$HT10)
med.fit <- lm(abba[abba$ELEV=="M",]$HT10~abba[abba$ELEV=="M",]$DBH10)
abline(med.fit)
plot(abba[abba$ELEV=="H",]$DBH10,abba[abba$ELEV=="H",]$HT10)
high.fit <- lm(abba[abba$ELEV == "H",]$HT10~abba[abba$ELEV=="H",]$DBH10)
abline(high.fit)

par(mfrow=c(1,3))
### Plot of ABBA at low elevation
plot(abba[abba$ELEV=="L",]$DBH10,abba[abba$ELEV=="L",]$HT10,pch="",xlab="DBH 2010",ylab="Height 2010",
	main="Abba at Low Elevation 2010, Height vs. DBH")
points(x=abba[abba$ELEV=="L"&abba$CRPOS10=="o",]$DBH10,
	y=abba[abba$ELEV=="L"&abba$CRPOS10=="o",]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="L"&abba$CRPOS10=="i",]$DBH10,
	y=abba[abba$ELEV=="L"&abba$CRPOS10=="i",]$HT10, pch = 3, col = "red")
points(x=abba[abba$ELEV=="L"&abba$CRPOS10=="c",]$DBH10,
	y=abba[abba$ELEV=="L"&abba$CRPOS10=="c",]$HT10, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="L"&abba$CRPOS10=="d",]$DBH10,
	y=abba[abba$ELEV=="L"&abba$CRPOS10=="d",]$HT10, pch = 1, col = "orange")
legend(20,5,pch = c(2,3,7,1), col=c("green","red","blue","orange"),legend = list("Overtopped",
	"Intermediate","Co-dominant","Dominant"))

### Plot of ABBA at mid elevation
plot(abba[abba$ELEV=="M",]$DBH10,abba[abba$ELEV=="M",]$HT10,pch="",xlab="DBH 2010",ylab="Height 2010",
	main="Abba at Mid Elevation 2010, Height vs. DBH")
points(x=abba[abba$ELEV=="M"&abba$CRPOS10=="o",]$DBH10,
	y=abba[abba$ELEV=="M"&abba$CRPOS10=="o",]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="M"&abba$CRPOS10=="i",]$DBH10,
	y=abba[abba$ELEV=="M"&abba$CRPOS10=="i",]$HT10, pch = 3, col = "red")
points(x=abba[abba$ELEV=="M"&abba$CRPOS10=="c",]$DBH10,
	y=abba[abba$ELEV=="M"&abba$CRPOS10=="c",]$HT10, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="M"&abba$CRPOS10=="d",]$DBH10,
	y=abba[abba$ELEV=="M"&abba$CRPOS10=="d",]$HT10, pch = 1, col = "orange")
legend(20,5,pch = c(2,3,7,1), col=c("green","red","blue","orange"),legend = list("Overtopped",
	"Intermediate","Co-dominant","Dominant"))

### Plot of ABBA at high elevation
plot(abba[abba$ELEV=="H",]$DBH10,abba[abba$ELEV=="H",]$HT10,pch="",xlab="DBH 2010",ylab="Height 2010",
	main="Abba at High Elevation 2010, Height vs. DBH")
points(x=abba[abba$ELEV=="H"&abba$CRPOS10=="o",]$DBH10,
	y=abba[abba$ELEV=="H"&abba$CRPOS10=="o",]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="H"&abba$CRPOS10=="i",]$DBH10,
	y=abba[abba$ELEV=="H"&abba$CRPOS10=="i",]$HT10, pch = 3, col = "red")
points(x=abba[abba$ELEV=="H"&abba$CRPOS10=="c",]$DBH10,
	y=abba[abba$ELEV=="H"&abba$CRPOS10=="c",]$HT10, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="H"&abba$CRPOS10=="d",]$DBH10,
	y=abba[abba$ELEV=="H"&abba$CRPOS10=="d",]$HT10, pch = 1, col = "orange")
legend(20,5,pch = c(2,3,7,1), col=c("green","red","blue","orange"),legend = list("Overtopped",
	"Intermediate","Co-dominant","Dominant"))

############################### 1998 #########################################################
par(mfrow=c(1,3))
### Plot of ABBA at low elevation
plot(abba[abba$ELEV=="L",]$DBH98,abba[abba$ELEV=="L",]$HT98,pch="",xlab="DBH 1998",ylab="Height 1998",
	main="Abba at Low Elevation 1998, Height vs. DBH")
points(x=abba[abba$ELEV=="L"&abba$CRPOS98=="o",]$DBH98,
	y=abba[abba$ELEV=="L"&abba$CRPOS98=="o",]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="L"&abba$CRPOS98=="i",]$DBH98,
	y=abba[abba$ELEV=="L"&abba$CRPOS98=="i",]$HT98, pch = 3, col = "red")
points(x=abba[abba$ELEV=="L"&abba$CRPOS98=="c",]$DBH98,
	y=abba[abba$ELEV=="L"&abba$CRPOS98=="c",]$HT98, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="L"&abba$CRPOS98=="d",]$DBH98,
	y=abba[abba$ELEV=="L"&abba$CRPOS98=="d",]$HT98, pch = 1, col = "orange")
legend(20,5,pch = c(2,3,7,1), col=c("green","red","blue","orange"),legend = list("Overtopped",
	"Intermediate","Co-dominant","Dominant"))
### Plot of ABBA at mid elevation
plot(abba[abba$ELEV=="M",]$DBH98,abba[abba$ELEV=="M",]$HT98,pch="",xlab="DBH 1998",ylab="Height 1998",
	main="Abba at Mid Elevation 1998, Height vs. DBH")
points(x=abba[abba$ELEV=="M"&abba$CRPOS98=="o",]$DBH98,
	y=abba[abba$ELEV=="M"&abba$CRPOS98=="o",]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="M"&abba$CRPOS98=="i",]$DBH98,
	y=abba[abba$ELEV=="M"&abba$CRPOS98=="i",]$HT98, pch = 3, col = "red")
points(x=abba[abba$ELEV=="M"&abba$CRPOS98=="c",]$DBH98,
	y=abba[abba$ELEV=="M"&abba$CRPOS98=="c",]$HT98, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="M"&abba$CRPOS98=="d",]$DBH98,
	y=abba[abba$ELEV=="M"&abba$CRPOS98=="d",]$HT98, pch = 1, col = "orange")
legend(20,5,pch = c(2,3,7,1), col=c("green","red","blue","orange"),legend = list("Overtopped",
	"Intermediate","Co-dominant","Dominant"))
### Plot of ABBA at high elevation
plot(abba[abba$ELEV=="H",]$DBH98,abba[abba$ELEV=="H",]$HT98,pch="",xlab="DBH 1998",ylab="Height 1998",
	main="Abba at High Elevation 1998, Height vs. DBH")
points(x=abba[abba$ELEV=="H"&abba$CRPOS98=="o",]$DBH98,
	y=abba[abba$ELEV=="H"&abba$CRPOS98=="o",]$HT98, pch = 2, col = "green")
points(x=abba[abba$ELEV=="H"&abba$CRPOS98=="i",]$DBH98,
	y=abba[abba$ELEV=="H"&abba$CRPOS98=="i",]$HT98, pch = 3, col = "red")
points(x=abba[abba$ELEV=="H"&abba$CRPOS98=="c",]$DBH98,
	y=abba[abba$ELEV=="H"&abba$CRPOS98=="c",]$HT98, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="H"&abba$CRPOS98=="d",]$DBH98,
	y=abba[abba$ELEV=="H"&abba$CRPOS98=="d",]$HT98, pch = 1, col = "orange")
legend(20,5,pch = c(2,3,7,1), col=c("green","red","blue","orange"),legend = list("Overtopped",
	"Intermediate","Co-dominant","Dominant"))


plot(1:10,pch=1:10)
