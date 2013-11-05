### 5/24
### Bole volume calculation
### Use Kozak(2004)Model 02 as a taper equation
### Split each tree into 100 sections, use the taper equation to predict the diameters at the different
### heights and sum the sections together with smalians formula
### NOTE :: Kozak model returns funny values for dbhs less than 1, so they are omitted here

pp <- read.csv("C:/Users/Noah/Dropbox/Noah/pp6.20.csv",header=T,sep=",")

### kozak taper equation, returns dbh, d, at specified height, h
kozak2004 <- function(D,H,species,h) {
	if(species == "ABBA") {
		a0 = 0.911
		a1 = 1.026
		a2 = -0.005
		b1 = 0.368
		b2 = -0.645
		b3 = 0.502
		b4 = 1.780
		b5 = 0.096
		b6 = -0.487
	}
	if(species == "PIRU") {
		a0 = 0.940
		a1 = 0.998
		a2 = 0.010
		b1 = 0.508
		b2 = -0.636
		b3 = 0.355
		b4 = 1.687
		b5 = 0.078
		b6 = -0.242
	}
	z <- h/H
	p <- 1.3/H
	Q <- 1-z^(1/3)
	X <- (1-(h/H)^(1/3))/(1-p^(1/3))
	d <- a0*(D^a1)*(H^a2)*X^(b1*(z^4)+b2*(1/exp(D/H))+b3*(X^0.1)+b4*(1/D)+b5*(H^Q)+b6*X)
	return(d);
}

### Smalian formula, D1 and D2 are diameters at each end of log, L is length
### returns volume in cubic m
smalian <- function(D1,D2,L) {
	D1 <- 0.00007854*(D1^2)
	D2 <- 0.00007854*(D2^2)
	volume = ((D1+D2)/2)*L
	return(volume)
}

### Honer eq 1965
honer <- function(D,H,species) {
	D <- 0.393700787*D
	H <- 3.2808399*H
	if(species == "ABBA") {
		a = 2.139
		b = 301.634
	}
	if(species == "PIRU") {
		a = 0.691
		b = 363.676
	}
	V = 0.0283168466*D^2/(a+(b/H))
	return(V)
}

###################### Bole Volume Formula ##################################################
bolevol <- function(dbh,ht,species) {
	increment <- ht/10
	heights <- seq(from=0,to=ht, by = increment)
	diams <- kozak2004(dbh,ht,species=species,heights)
	volume = 0
	for(j in 1:10) { volume=volume+smalian(diams[[j]],diams[[j+1]],increment) } 
	return(volume)
}
#################  Tree volume calculation 1998 #################################################
pp$KOZAK98 <- rep(NA, nrow(pp))
pp$HONER98 <- rep(NA, nrow(pp))

for(i in 1:nrow(pp)) {
	if(!is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$DBH98[[i]]>=0.9) {
		if(pp$SPECIES[[i]] == "ABBA" | pp$SPECIES[[i]] == "PIRU") {
		pp$KOZAK98[[i]] <- bolevol(pp$DBH98[[i]],pp$HT98[[i]],pp$SPECIES[[i]])
############################### Honer check 
		pp$HONER98[[i]] <- honer(pp$DBH98[[i]],pp$HT98[[i]],species = pp$SPECIES[[i]])
		}
	}
}
################ Use the honer equation for trees with DBH < 0.9
for(i in 1:nrow(pp)) {
	if(!is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$DBH98[[i]]<0.9) {
		if(pp$SPECIES[[i]] == "ABBA" | pp$SPECIES[[i]] == "PIRU") {
		pp$HONER98[[i]] <- honer(pp$DBH98[[i]],pp$HT98[[i]],species = pp$SPECIES[[i]])
		}
	}
}
windows()
par(mfrow=c(1,2))
plot(pp$DBH98,pp$BV98,xlab="DBH98",ylab="Bole Volume (m^3)",main="ABBA and PIRU Bole Volumes 1998,
Kozak(2004) Model 02, DBHs >= 0.9")
plot(pp$DBH98,pp$HONER98,xlab="DBH98",ylab="Bole Volume (m^3)",main="ABBA and PIRU Bole Volumes 1998,
Honer Eq, All DBHs")

#################  Tree volume calculation 2010 ##############################################
pp$KOZAK10 <- rep(NA, nrow(pp))
pp$HONER10 <- rep(NA, nrow(pp))

for(i in 1:nrow(pp)) {
	if(!is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$DBH10[[i]]>=0.9) {
		if(pp$SPECIES[[i]] == "ABBA" | pp$SPECIES[[i]] == "PIRU") {
		pp$KOZAK10[[i]] <- bolevol(pp$DBH10[[i]],pp$HT10[[i]],pp$SPECIES[[i]])
############################### Honer check 
		pp$HONER10[[i]] <- honer(pp$DBH10[[i]],pp$HT10[[i]],species = pp$SPECIES[[i]])
		}
	}
}
################ Use the honer equation for trees with DBH < 0.9
for(i in 1:nrow(pp)) {
	if(!is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$DBH10[[i]]<0.9) {
		if(pp$SPECIES[[i]] == "ABBA" | pp$SPECIES[[i]] == "PIRU") {
		pp$HONER10[[i]] <- honer(pp$DBH10[[i]],pp$HT10[[i]],species = pp$SPECIES[[i]])
		}
	}
}
windows()
par(mfrow=c(1,2))
plot(pp$DBH10,pp$BV10,xlab="DBH10",ylab="Bole Volume (m^3)",main="ABBA and PIRU Bole Volumes 2010,
Kozak(2004) Model 02, DBHs >= 0.9")
plot(pp$DBH10,pp$HONER10,xlab="DBH10",ylab="Bole Volume (m^3)",main="ABBA and PIRU Bole Volumes 2010,
Honer Eq, All DBHs")











