### 5/18
### Comparing fits for canopy effect on ht vs dbh

pp <- read.csv("C:/Users/Noah/Desktop/canopy.csv",header=T,sep=",")
abba <- subset(pp, SPECIES == "ABBA")
rmse <- function(obs,pred) sqrt(mean((obs-pred)^2))
##############################  1998  #########################################
########## linear fits 
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

########## Nonlinear fits 
lowabove98.mod <- nls(abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$HT98~
	a*abba[abba$ELEV=="L"&abba$HT98>=abba$CANHT98,]$DBH98^b,
	start = list(a=1,b=2))
