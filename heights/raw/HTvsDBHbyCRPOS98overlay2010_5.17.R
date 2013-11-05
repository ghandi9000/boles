### 5/17
### DBH vs HT for ABBA at three elevations
### colored points show 2010 data with symbols for crown position
### black points show 1998 data
### fits are shown for data from both sample years

par(mfrow=c(1,3))
### Plot of ABBA at low elevation
plot(abba[abba$ELEV=="L",]$DBH10,abba[abba$ELEV=="L",]$HT10,pch="",xlab="DBH",ylab="Height",
	main="Abba at Low Elevation, Height vs. DBH
	Colored points are from 2010, black points from 1998")
points(x=abba[abba$ELEV=="L"&abba$CRPOS10=="o",]$DBH10,
	y=abba[abba$ELEV=="L"&abba$CRPOS10=="o",]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="L"&abba$CRPOS10=="i",]$DBH10,
	y=abba[abba$ELEV=="L"&abba$CRPOS10=="i",]$HT10, pch = 3, col = "red")
points(x=abba[abba$ELEV=="L"&abba$CRPOS10=="c",]$DBH10,
	y=abba[abba$ELEV=="L"&abba$CRPOS10=="c",]$HT10, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="L"&abba$CRPOS10=="d",]$DBH10,
	y=abba[abba$ELEV=="L"&abba$CRPOS10=="d",]$HT10, pch = 1, col = "orange")
points(x=abba[abba$ELEV=="L",]$DBH98,
	y=abba[abba$ELEV=="L",]$HT98, pch = 20, col = "black")

legend(20,8,pch = c(20,20,20,2,3,7,1), col=c("purple","brown","black","green","red","blue","orange"),
	legend = list("1998 Fit","2010 Fit","1998 points","Overtopped",
	"Intermediate","Co-dominant","Dominant"))
### add the fits to the plot
lines(sort(dbh2),sort(fitted.values(low98.mod)),lwd=2,col="purple")
lines(sort(dbh),sort(fitted.values(low98.fit)),lwd=2,col="purple")
lines(sort(dbh3),sort(fitted.values(low10.mod)),lwd=2,col="brown")
lines(sort(dbh4),sort(fitted.values(low10.fit)),lwd=2,col="brown")

### Plot of ABBA at mid elevation
plot(abba[abba$ELEV=="M",]$DBH10,abba[abba$ELEV=="M",]$HT10,pch="",xlab="DBH",ylab="Height",
	main="Abba at Mid Elevation, Height vs. DBH
	Colored points are from 2010, black points from 1998")
points(x=abba[abba$ELEV=="M"&abba$CRPOS10=="o",]$DBH10,
	y=abba[abba$ELEV=="M"&abba$CRPOS10=="o",]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="M"&abba$CRPOS10=="i",]$DBH10,
	y=abba[abba$ELEV=="M"&abba$CRPOS10=="i",]$HT10, pch = 3, col = "red")
points(x=abba[abba$ELEV=="M"&abba$CRPOS10=="c",]$DBH10,
	y=abba[abba$ELEV=="M"&abba$CRPOS10=="c",]$HT10, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="M"&abba$CRPOS10=="d",]$DBH10,
	y=abba[abba$ELEV=="M"&abba$CRPOS10=="d",]$HT10, pch = 1, col = "orange")
points(x=abba[abba$ELEV=="M",]$DBH98,
	y=abba[abba$ELEV=="M",]$HT98, pch = 20, col = "black")
legend(20,8,pch = c(20,20,20,2,3,7,1), col=c("purple","brown","black","green","red","blue","orange"),
	legend = list("1998 Fit","2010 Fit","1998 points","Overtopped",
	"Intermediate","Co-dominant","Dominant"))
### add the fits to the plot
lines(sort(dbhmid2),sort(fitted.values(mid98.mod)),lwd=2,col="purple")
lines(sort(dbhmid),sort(fitted.values(mid98.fit)),lwd=2,col="purple")
lines(sort(dbhmid3),sort(fitted.values(mid10.mod)),lwd=2,col="brown")
lines(sort(dbhmid4),sort(fitted.values(mid10.fit)),lwd=2,col="brown")

### Plot of ABBA at high elevation
plot(abba[abba$ELEV=="H",]$DBH10,abba[abba$ELEV=="H",]$HT10,pch="",xlab="DBH",ylab="Height",
	main="Abba at High Elevation, Height vs. DBH
	Colored points are from 2010, black points from 1998")
points(x=abba[abba$ELEV=="H"&abba$CRPOS10=="o",]$DBH10,
	y=abba[abba$ELEV=="H"&abba$CRPOS10=="o",]$HT10, pch = 2, col = "green")
points(x=abba[abba$ELEV=="H"&abba$CRPOS10=="i",]$DBH10,
	y=abba[abba$ELEV=="H"&abba$CRPOS10=="i",]$HT10, pch = 3, col = "red")
points(x=abba[abba$ELEV=="H"&abba$CRPOS10=="c",]$DBH10,
	y=abba[abba$ELEV=="H"&abba$CRPOS10=="c",]$HT10, pch = 7, col = "blue")
points(x=abba[abba$ELEV=="H"&abba$CRPOS10=="d",]$DBH10,
	y=abba[abba$ELEV=="H"&abba$CRPOS10=="d",]$HT10, pch = 1, col = "orange")
points(x=abba[abba$ELEV=="H",]$DBH98,
	y=abba[abba$ELEV=="H",]$HT98, pch = 20, col = "black")
legend(20,8,pch = c(20,20,20,2,3,7,1), col=c("purple","brown","black","green","red","blue","orange"),
	legend = list("1998 Fit","2010 Fit","1998 points","Overtopped",
	"Intermediate","Co-dominant","Dominant"))
### add the fits to the plot
lines(sort(dbhhigh2),sort(fitted.values(high98.mod)),lwd=2,col="purple")
lines(sort(dbhhigh),sort(fitted.values(high98.fit)),lwd=2,col="purple")
lines(sort(dbhhigh3),sort(fitted.values(high10.mod)),lwd=2,col="brown")
lines(sort(dbhhigh4),sort(fitted.values(high10.fit)),lwd=2,col="brown")

### Fits for 1998 #############################################################################
### LOW ###
ht2 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98 >= 2.7&!is.na(abba$HT98),]$HT98
dbh2 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98 >= 2.7&!is.na(abba$HT98),]$DBH98
ht <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98 < 2.7&!is.na(abba$HT98),]$HT98
dbh <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98 < 2.7&!is.na(abba$HT98),]$DBH98
### nonlinear part
low98.mod <- nls(ht2~a*dbh2^b,start = list(a=1.4,b=0.7))
plot(dbh2,ht2)
lines(sort(dbh2),sort(fitted.values(low98.mod)),lwd=2,col="purple")
low98.mod.rmse <- rmse(ht2,fitted.values(low98.mod))
### linear part
low98.fit <- lm(ht~dbh)
lines(sort(dbh),sort(fitted.values(low98.fit)),lwd=2,col="purple")

### MID ###
htmid2 <- abba[abba$ELEV=="M"&!is.na(abba$DBH98)&abba$DBH98 >= 2.7&!is.na(abba$HT98),]$HT98
dbhmid2 <- abba[abba$ELEV=="M"&!is.na(abba$DBH98)&abba$DBH98 >= 2.7&!is.na(abba$HT98),]$DBH98
htmid <- abba[abba$ELEV=="M"&!is.na(abba$DBH98)&abba$DBH98 < 2.7&!is.na(abba$HT98),]$HT98
dbhmid <- abba[abba$ELEV=="M"&!is.na(abba$DBH98)&abba$DBH98 < 2.7&!is.na(abba$HT98),]$DBH98
### nonlinear part
mid98.mod <- nls(htmid2~a*dbhmid2^b,start = list(a=1.4,b=0.7))
plot(dbhmid2,htmid2)
lines(sort(dbhmid2),sort(fitted.values(mid98.mod)),lwd=2,col="purple")
mid98.mod.rmse <- rmse(htmid2,fitted.values(mid98.mod))
### linear part
mid98.fit <- lm(htmid~dbhmid)
lines(sort(dbhmid),sort(fitted.values(mid98.fit)),lwd=2,col="purple")

### HIGH ###
hthigh2 <- abba[abba$ELEV=="H"&!is.na(abba$DBH98)&abba$DBH98 >= 2.7&!is.na(abba$HT98),]$HT98
dbhhigh2 <- abba[abba$ELEV=="H"&!is.na(abba$DBH98)&abba$DBH98 >= 2.7&!is.na(abba$HT98),]$DBH98
hthigh <- abba[abba$ELEV=="H"&!is.na(abba$DBH98)&abba$DBH98 < 2.7&!is.na(abba$HT98),]$HT98
dbhhigh <- abba[abba$ELEV=="H"&!is.na(abba$DBH98)&abba$DBH98 < 2.7&!is.na(abba$HT98),]$DBH98
### nonlinear part
high98.mod <- nls(hthigh2~a*dbhhigh2^b,start = list(a=1.4,b=0.7))
plot(dbhhigh2,hthigh2)
lines(sort(dbhhigh2),sort(fitted.values(high98.mod)),lwd=2,col="purple")
high98.mod.rmse <- rmse(hthigh2,fitted.values(high98.mod))
### linear part
high98.fit <- lm(hthigh~dbhhigh)
lines(sort(dbhhigh),sort(fitted.values(high98.fit)),lwd=2,col="purple")

### Fits for 2010 #############################################################################
### LOW ###
ht3 <- abba[abba$ELEV=="L"&!is.na(abba$DBH10)&abba$DBH10 >= 2.7&!is.na(abba$HT10),]$HT10
dbh3 <- abba[abba$ELEV=="L"&!is.na(abba$DBH10)&abba$DBH10 >= 2.7&!is.na(abba$HT10),]$DBH10
ht4 <- abba[abba$ELEV=="L"&!is.na(abba$DBH10)&abba$DBH10 < 2.7&!is.na(abba$HT10),]$HT10
dbh4 <- abba[abba$ELEV=="L"&!is.na(abba$DBH10)&abba$DBH10 < 2.7&!is.na(abba$HT10),]$DBH10
### nonlinear part
low10.mod <- nls(ht3~a*dbh3^b,start = list(a=1.4,b=0.7))
plot(dbh3,ht3)
lines(sort(dbh3),sort(fitted.values(low10.mod)),lwd=2,col="brown")
low10.mod.rmse <- rmse(ht3,fitted.values(low10.mod))
### linear part
low10.fit <- lm(ht4~dbh4)
lines(sort(dbh4),sort(fitted.values(low10.fit)),lwd=2,col="brown")
### MID ###
htmid3 <- abba[abba$ELEV=="M"&!is.na(abba$DBH10)&abba$DBH10 >= 2.7&!is.na(abba$HT10),]$HT10
dbhmid3 <- abba[abba$ELEV=="M"&!is.na(abba$DBH10)&abba$DBH10 >= 2.7&!is.na(abba$HT10),]$DBH10
htmid4 <- abba[abba$ELEV=="M"&!is.na(abba$DBH10)&abba$DBH10 < 2.7&!is.na(abba$HT10),]$HT10
dbhmid4 <- abba[abba$ELEV=="M"&!is.na(abba$DBH10)&abba$DBH10 < 2.7&!is.na(abba$HT10),]$DBH10
### nonlinear part
mid10.mod <- nls(htmid3~a*dbhmid3^b,start = list(a=1.4,b=0.7))
plot(dbhmid3,htmid3)
lines(sort(dbhmid3),sort(fitted.values(mid10.mod)),lwd=2,col="brown")
mid10.mod.rmse <- rmse(htmid3,fitted.values(mid10.mod))
### linear part
mid10.fit <- lm(htmid4~dbhmid4)
lines(sort(dbhmid4),sort(fitted.values(mid10.fit)),lwd=2,col="brown")
### HIGH ###
hthigh3 <- abba[abba$ELEV=="H"&!is.na(abba$DBH10)&abba$DBH10 >= 2.7&!is.na(abba$HT10),]$HT10
dbhhigh3 <- abba[abba$ELEV=="H"&!is.na(abba$DBH10)&abba$DBH10 >= 2.7&!is.na(abba$HT10),]$DBH10
hthigh4 <- abba[abba$ELEV=="H"&!is.na(abba$DBH10)&abba$DBH10 < 2.7&!is.na(abba$HT10),]$HT10
dbhhigh4 <- abba[abba$ELEV=="H"&!is.na(abba$DBH10)&abba$DBH10 < 2.7&!is.na(abba$HT10),]$DBH10
### nonlinear part
high10.mod <- nls(hthigh3~a*dbhhigh3^b,start = list(a=1.4,b=0.7))
plot(dbhhigh3,hthigh3)
lines(sort(dbhhigh3),sort(fitted.values(high10.mod)),lwd=2,col="brown")
high10.mod.rmse <- rmse(hthigh3,fitted.values(high10.mod))
### linear part
high10.fit <- lm(hthigh4~dbhhigh4)
lines(sort(dbhhigh4),sort(fitted.values(high10.fit)),lwd=2,col="brown")









