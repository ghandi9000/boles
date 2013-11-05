### Bole Volume Calculations
### First uses the Kozak/Clark/Honer equations to calculate the bole volume of large trees,
### then extrapolate to small trees using standard power function
pp <- read.csv("~/work/data/data/boles/prepped-for-bole-calculation.csv")

################################################
######						######
######		Formulas			######
######						######
################################################
### kozak taper equation, returns diameter, d, at specified height, h
### funny volumes below 0.9 DBH, so small trees excluded
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
### this version of the honer equation takes dbh in cm and height in m
### thus returning cubic meter
honer2 <- function(dbh,height,species) {
	if(species == "BECO") { a0 = 2.222; a1 = 91.554; a2 = 0.0043222 }
	if(species == "BEAL") { a0 = 1.449; a1 = 105.081; a2 = 0.004320 }
	if(species == "ACSA" | species == "ACSP" | species == "ACPE") {
		a0 =1.046; a1 = 117.035; a2 = 0.004334 }
	if(species == "FAGR") {
		d1.3 <- dbh/(1-0.04365*0.145);
		dbh <- d1.3;
		a0=0.959; a1 = 102.056; a2 = 0.004334 }
	if(species == "PRPE" | species == "SOAM") { a0 = 0.033; a1 = 119.889; a2 = 0.004334 }
	V = (a2*dbh^2)/(a0+(a1/height))
	return(V)
}

###################### Bole Volume Formula For Kozak BV ##########################
bolevol <- function(dbh,ht,species) {
	increment <- ht/10
	heights <- seq(from=0,to=ht, by = increment)
	diams <- kozak2004(dbh,ht,species=species,heights)
	volume = 0
	for(j in 1:10) { volume=volume+smalian(diams[[j]],diams[[j+1]],increment) }
	return(volume)
}

###################### Clark BV Equation #########################################
### Requires trees to be 17.3 feet tall and 5 in diameter
### input units are dbh in cm, height in m, output is in m^3

clark <- function(dbh,height,species) {
	##### Species Parameters ########################
	if(species=="PRPE" | species=="SOAM") { a1 = 0.92487; b1 = -0.89867;  r = 37.12714; c = 0.48776; e = 1.50579;
		p = 6.18866; b = 1.64261; a = 0.55071 }
###	if(species=="ABBA" | species=="PIRU") { a1 = 0.92487; b1 = -0.89867;  r = 31.66250; c = 0.57402; e = 110.96;
###		p = 8.573; b = 2.36238; a = 0.68464 } ### using the coefficients for loblolly pine to get comparisons for ABBA and PIRU

	if(species=="ACPE" | species=="ACSA" | species=="ACSP") { a1 = 0.93991; b1 = -1.62226; r = 22.00135; c = 0.45472;
		e = 166.1; p = 7.31546; b = 1.17064; a = 0.27213 }
	if(species=="FAGR") { a1 = 0.91141; b1 = -0.6673; r = 44.36826; c = 1.22158; e = 79.44636;
		p = 6.36236; b = 1.11382; a = 0.14312 }
	if(species=="BEAL" | species=="BECO" | species=="BEPA") { a1 = 0.85516; b1 = -0.00134; r = 49.41385; c = 1.01241;
		e = -91.82769; p = 11.23179; b = 1.19704; a = 0.23928 }
	###if(species=="SOAM") { a1 = 0.91531; b1 = -0.96788; r = 0.64600; c = 0.49680; e = 127.87;
	###	p = 7.52915; b = 1.49287; a = 0.47222 }

	### Basic Symbols ###############################
	### V <- stem volumen between L and U in cubic ft.
	### L <- lower height of interest
	### U <- upper height of interest
	### D <- diameter in inches at 4.5 ft
	### H <- total ht of tree in FT
	### F <- diameter at 17.3 ft above ground in inches, DOB17 = D(a+b(17.3/H)^2) where a,b are sp. coefs
	D <- dbh*0.393700787 ### convert dbh in cm to dbh in in.
	L <- 0
	U <- height*3.2808399 ### convert height in meters to height in feet
	H <- U
	F <- D*(a1+b1*(17.3/H)^2)
	### Combined Variables #########################
	L1 <- max(L,0)
	U1 <- min(U,4.5)
	L2 <- max(L,4.5)
	U2 <- min(U,17.3)
	L3 <- max(L,17.3)
	U3 <- min(U,H)
	G <- (1-4.5/H)^r
	W <- (c + e/D^3)/(1-G)
	X <- (1-4.5/H)^p
	Y <- (1-17.3/H)^p
	Z <- (D^2-F^2)/(X-Y)
	T <- (D^2-Z*X)

	### Indicator Variables ########################
	I1 <- if(L < 4.5) I1 <- 1 else(I1 <- 0)
	I2 <- if(L < 17.3) I2 <- 1 else(I2 <- 0)
	I3 <- if(U > 4.5) I3 <- 1 else(I3 <- 0)
	I4 <- if(U > 17.3) I4 <- 1 else(I4 <- 0)
	I5 <- if((L3-17.3)<a*(H-17.3)) I5 <- 1 else(I5 <- 0)
	I6 <- if((U3-17.3)<a*(H-17.3)) I6 <- 1 else(I6 <- 0)

	### main stem volume calculation ################
	V<- 0.005454154*(I1*D^2*((1-G*W)*(U1-L1)+W*((1-L1/H)^r*(H-L1) -
	(1-U1/H)^r*(H-U1))/(r+1))
	+ I2*I3*(T*(U2-L2)+Z*((1-L2/H)^p*(H-L2) -
	(1-U2/H)^p*(H-U2))/(p+1))
	+ I4*F^2*(b*(U3-L3)-b*((U3-17.3)^2-(L3-17.3)^2)/(H-17.3) +
	(b/3)*((U3-17.3)^3-(L3-17.3)^3)/(H-17.3)^2 +
	I5*(1/3)*((1-b)/a^2)*(a*(H-17.3)-(L3-17.3))^3/(H-17.3)^2 -
	I6*(1/3)*((1-b)/a^2)*(a*(H-17.3)-(U3-17.3))^3/(H-17.3)^2))
	return(V*0.0283168466)
}

#################  Kozak BV Calculation  ##############################################
pp$KOZAK86 <- rep(NA, nrow(pp))
pp$HONER86 <- rep(NA, nrow(pp))
pp$KOZAK87 <- rep(NA, nrow(pp))
pp$HONER87 <- rep(NA, nrow(pp))
pp$KOZAK98 <- rep(NA, nrow(pp))
pp$HONER98 <- rep(NA, nrow(pp))
pp$KOZAK10 <- rep(NA, nrow(pp))
pp$HONER10 <- rep(NA, nrow(pp))

for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]]=="ABBA") {	   ############ works for trees greater than 1.3 DBH and greater than 1.3 HT
		if(!is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$DBH86[[i]]>1.3 & pp$HT86[[i]] > 1.3) {
			pp$KOZAK86[[i]] <- bolevol(pp$DBH86[[i]],pp$HT86[[i]],pp$SPECIES[[i]]);
		}
		if(!is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$DBH87[[i]]>1.3 & pp$HT87[[i]] > 1.3) {
			pp$KOZAK87[[i]] <- bolevol(pp$DBH87[[i]],pp$HT87[[i]],pp$SPECIES[[i]]);
		}
		if(!is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$DBH98[[i]]>1.3 & pp$HT98[[i]] > 1.3) {
			pp$KOZAK98[[i]] <- bolevol(pp$DBH98[[i]],pp$HT98[[i]],pp$SPECIES[[i]]);
		}
		if(!is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$DBH10[[i]]>1.3 & pp$HT10[[i]] > 1.3) {
			pp$KOZAK10[[i]] <- bolevol(pp$DBH10[[i]],pp$HT10[[i]],pp$SPECIES[[i]]);
		}
	}
	if(pp$SPECIES[[i]]=="PIRU") {	   ############ works for trees greater than 1.3 DBH and greater than 1.5 HT
		if(!is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$DBH86[[i]]>1.3 & pp$HT86[[i]] > 1.5) {
			pp$KOZAK86[[i]] <- bolevol(pp$DBH86[[i]],pp$HT86[[i]],pp$SPECIES[[i]]);
		}
		if(!is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$DBH87[[i]]>1.3 & pp$HT87[[i]] > 1.5) {
			pp$KOZAK87[[i]] <- bolevol(pp$DBH87[[i]],pp$HT87[[i]],pp$SPECIES[[i]]);
		}
		if(!is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$DBH98[[i]]>1.3 & pp$HT98[[i]] > 1.5) {
			pp$KOZAK98[[i]] <- bolevol(pp$DBH98[[i]],pp$HT98[[i]],pp$SPECIES[[i]]);
		}
		if(!is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$DBH10[[i]]>1.3 & pp$HT10[[i]] > 1.5) {
			pp$KOZAK10[[i]] <- bolevol(pp$DBH10[[i]],pp$HT10[[i]],pp$SPECIES[[i]]);
		}
	}
}
################ Use the honer equation for all trees
for(i in 1:nrow(pp)) {
	if(!is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]])) {
		if(pp$SPECIES[[i]] == "ABBA" | pp$SPECIES[[i]] == "PIRU") {
		pp$HONER86[[i]] <- honer(pp$DBH86[[i]],pp$HT86[[i]],species = pp$SPECIES[[i]])
		};
	}
	if(!is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]])) {
		if(pp$SPECIES[[i]] == "ABBA" | pp$SPECIES[[i]] == "PIRU") {
		pp$HONER87[[i]] <- honer(pp$DBH87[[i]],pp$HT87[[i]],species = pp$SPECIES[[i]])
		};
	}
	if(!is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]])) {
		if(pp$SPECIES[[i]] == "ABBA" | pp$SPECIES[[i]] == "PIRU") {
		pp$HONER98[[i]] <- honer(pp$DBH98[[i]],pp$HT98[[i]],species = pp$SPECIES[[i]])
		};
	}
	if(!is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]])) {
		if(pp$SPECIES[[i]] == "ABBA" | pp$SPECIES[[i]] == "PIRU") {
		pp$HONER10[[i]] <- honer(pp$DBH10[[i]],pp$HT10[[i]],species = pp$SPECIES[[i]])
		};
	}
}

##############################################################################
######		Fit to power function and use for small DBHs		######
##############################################################################

### Make a working dataset for ABBA with DBHs and BVs from all years in one column,
### but use each tagged individual only once
abba <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK86) | ### 86 trees
	pp$SPECIES=="ABBA" & !is.na(pp$KOZAK87) | ### 87 trees
	pp$SPECIES=="ABBA" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) | ### 98 trees
	pp$SPECIES=="ABBA" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK86),]$KOZAK86
ht86 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK86),]$HT86
elevs <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK86),]$ELEV
plots <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK86),]$PLOT
canht <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK86),]$CANHT86
year <- rep(86,length(bv86))
abba86 <- data.frame(plots,ht86,bv86,canht,elevs,year)
names(abba86) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK87),]$KOZAK87
ht87 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK87),]$HT87
elevs <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK87),]$ELEV
plots <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK87),]$PLOT
canht <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK87),]$CANHT87
year <- rep(87,length(bv87))
abba87 <- data.frame(plots,ht87,bv87,canht,elevs,year)
names(abba87) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$KOZAK98
ht98 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$HT98
elevs <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$ELEV
plots <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$PLOT
canht <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$CANHT98
year <- rep(98,length(bv98))
abba98 <- data.frame(plots,ht98,bv98,canht,elevs,year)
names(abba98) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$KOZAK10
ht10 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$HT10
elevs <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$ELEV
plots <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$PLOT
canht <- pp[pp$SPECIES=="ABBA" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$CANHT10
year <- rep(10,length(bv10))
abba10 <- data.frame(plots,ht10,bv10,canht,elevs,year)
names(abba10) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

abba <- rbind(abba86,abba87)
abba <- rbind(abba,abba98)
abba <- rbind(abba,abba10)
abba <- abba[abba$PLOT > 3,]
### remove large outlier tree that was dead at the time of measurement anyway
abba <- abba[abba$BV!=max(abba$BV),]

### nls fit abba
abbabv <- abba$BV
abbaht <- abba$HT
abba.mod <- nls(abbabv~a*abbaht^b, start=list(a=.1,b=.2))
plot(abba$HT,abba$BV)
curve(coef(abba.mod)[[1]]*x^coef(abba.mod)[[2]],add=T,col="red")

### Make a working dataset for PIRU with DBHs and BVs from all years in one column,
### but use each tagged individual only once
piru <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK86) | ### 86 trees
	pp$SPECIES=="PIRU" & !is.na(pp$KOZAK87) | ### 87 trees
	pp$SPECIES=="PIRU" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) | ### 98 trees
	pp$SPECIES=="PIRU" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK86),]$KOZAK86
ht86 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK86),]$HT86
elevs <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK86),]$ELEV
plots <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK86),]$PLOT
canht <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK86),]$CANHT86
year <- rep(86,length(bv86))
piru86 <- data.frame(plots,ht86,bv86,canht,elevs,year)
names(piru86) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK87),]$KOZAK87
ht87 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK87),]$HT87
elevs <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK87),]$ELEV
plots <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK87),]$PLOT
canht <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK87),]$CANHT87
year <- rep(87,length(bv87))
piru87 <- data.frame(plots,ht87,bv87,canht,elevs,year)
names(piru87) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$KOZAK98
ht98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$HT98
elevs <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$ELEV
plots <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$PLOT
canht <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK98) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87),]$CANHT98
year <- rep(98,length(bv98))
piru98 <- data.frame(plots,ht98,bv98,canht,elevs,year)
names(piru98) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$KOZAK10
ht10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$HT10
elevs <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$ELEV
plots <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$PLOT
canht <- pp[pp$SPECIES=="PIRU" & !is.na(pp$KOZAK10) & is.na(pp$KOZAK86) & is.na(pp$KOZAK87) & is.na(pp$KOZAK98),]$CANHT10
year <- rep(10,length(bv10))
piru10 <- data.frame(plots,ht10,bv10,canht,elevs,year)
names(piru10) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

piru <- rbind(piru86,piru87)
piru <- rbind(piru,piru98)
piru <- rbind(piru,piru10)
piru <- piru[piru$PLOT > 3,]

### nls fit piru
pirubv <- piru$BV
piruht <- piru$HT
piru.mod <- nls(pirubv~a*piruht^b, start=list(a=.1,b=.2))
plot(piru$HT,piru$BV)
curve(coef(piru.mod)[[1]]*x^coef(piru.mod)[[2]],add=T,col="red")

##############################################################################
######		Use fits to estimate BV of small ABBAs&PIRUs		######
##############################################################################

for(i in 1:nrow(pp)) {
 	if(pp$SPECIES[[i]]=="ABBA") {
		if(!is.na(pp$HT86[[i]])& pp$HT86[[i]] <= 1.3 |
		!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]])& pp$DBH86[[i]] <= 1.3) {
			pp$KOZAK86[[i]] <- coef(abba.mod)[[1]]*pp$HT86[[i]]^coef(abba.mod)[[2]];
		}
		if(!is.na(pp$HT87[[i]])& pp$HT87[[i]] <= 1.3 |
		!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]])& pp$DBH87[[i]] <= 1.3) {
			pp$KOZAK87[[i]] <- coef(abba.mod)[[1]]*pp$HT87[[i]]^coef(abba.mod)[[2]];
		}
		if(!is.na(pp$HT98[[i]])& pp$HT98[[i]] <= 1.3 |
		!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]])& pp$DBH98[[i]] <= 1.3) {
			pp$KOZAK98[[i]] <- coef(abba.mod)[[1]]*pp$HT98[[i]]^coef(abba.mod)[[2]];
		}
		if(!is.na(pp$HT10[[i]])& pp$HT10[[i]] <= 1.3 |
		!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]])& pp$DBH10[[i]] <= 1.3) {
			pp$KOZAK10[[i]] <- coef(abba.mod)[[1]]*pp$HT10[[i]]^coef(abba.mod)[[2]];
		}
	}
 	if(pp$SPECIES[[i]]=="PIRU") {
		if(!is.na(pp$HT86[[i]])& pp$HT86[[i]]<= 1.3 |
		!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]])& pp$DBH86[[i]] <= 1.5) {
			pp$KOZAK86[[i]] <- coef(piru.mod)[[1]]*pp$HT86[[i]]^coef(piru.mod)[[2]];
		}
		if(!is.na(pp$HT87[[i]])& pp$HT87[[i]] <= 1.3 |
		!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]])& pp$DBH87[[i]] <= 1.5) {
			pp$KOZAK87[[i]] <- coef(piru.mod)[[1]]*pp$HT87[[i]]^coef(piru.mod)[[2]];
		}
		if(!is.na(pp$HT98[[i]])& pp$HT98[[i]] <= 1.3 |
		!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]])& pp$DBH98[[i]] <= 1.5) {
			pp$KOZAK98[[i]] <- coef(piru.mod)[[1]]*pp$HT98[[i]]^coef(piru.mod)[[2]];
		}
		if(!is.na(pp$HT10[[i]])& pp$HT10[[i]] <= 1.3 |
		!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]])& pp$DBH10[[i]] <= 1.5) {
			pp$KOZAK10[[i]] <- coef(piru.mod)[[1]]*pp$HT10[[i]]^coef(piru.mod)[[2]];
		}
	}
}

##############################################################################
######		Clark Bole Volume Calculations				######
##############################################################################
pp$CLARK86 <- rep(NA, nrow(pp))
pp$CLARK87 <- rep(NA, nrow(pp))
pp$CLARK98 <- rep(NA, nrow(pp))
pp$CLARK10 <- rep(NA, nrow(pp))

for(i in 1:nrow(pp)) {
	if(!is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$HT86[[i]]>=5.28) {
		if(pp$SPECIES[[i]] == "ACSA" | pp$SPECIES[[i]] == "ACPE" | pp$SPECIES[[i]] == "ACSP" |
		pp$SPECIES[[i]] == "BECO" | pp$SPECIES[[i]] == "BEAL" | pp$SPECIES[[i]] == "FAGR" |
		pp$SPECIES[[i]] == "PRPE" | pp$SPECIES[[i]] == "SOAM") {
			pp$CLARK86[[i]] <- clark(pp$DBH86[[i]],pp$HT86[[i]],pp$SPECIES[[i]])
			pp$HONER86[[i]] <- honer2(pp$DBH86[[i]],pp$HT86[[i]],species = pp$SPECIES[[i]]);
		}
	}
	if(!is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$HT87[[i]]>=5.28) {
		if(pp$SPECIES[[i]] == "ACSA" | pp$SPECIES[[i]] == "ACPE" | pp$SPECIES[[i]] == "ACSP" |
		pp$SPECIES[[i]] == "BECO" | pp$SPECIES[[i]] == "BEAL" | pp$SPECIES[[i]] == "FAGR" |
		pp$SPECIES[[i]] == "PRPE" | pp$SPECIES[[i]] == "SOAM") {
			pp$CLARK87[[i]] <- clark(pp$DBH87[[i]],pp$HT87[[i]],pp$SPECIES[[i]])
			pp$HONER87[[i]] <- honer2(pp$DBH87[[i]],pp$HT87[[i]],species = pp$SPECIES[[i]]);
		}
	}
	if(!is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$HT98[[i]]>=5.28) {
		if(pp$SPECIES[[i]] == "ACSA" | pp$SPECIES[[i]] == "ACPE" | pp$SPECIES[[i]] == "ACSP" |
		pp$SPECIES[[i]] == "BECO" | pp$SPECIES[[i]] == "BEAL" | pp$SPECIES[[i]] == "FAGR" |
		pp$SPECIES[[i]] == "PRPE" | pp$SPECIES[[i]] == "SOAM") {
			pp$CLARK98[[i]] <- clark(pp$DBH98[[i]],pp$HT98[[i]],pp$SPECIES[[i]])
			pp$HONER98[[i]] <- honer2(pp$DBH98[[i]],pp$HT98[[i]],species = pp$SPECIES[[i]]);
		}
	}
	if(!is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$HT10[[i]]>=5.28) {
		if(pp$SPECIES[[i]] == "ACSA" | pp$SPECIES[[i]] == "ACPE" | pp$SPECIES[[i]] == "ACSP" |
		pp$SPECIES[[i]] == "BECO" | pp$SPECIES[[i]] == "BEAL" | pp$SPECIES[[i]] == "FAGR" |
		pp$SPECIES[[i]] == "PRPE" | pp$SPECIES[[i]] == "SOAM") {
			pp$CLARK10[[i]] <- clark(pp$DBH10[[i]],pp$HT10[[i]],pp$SPECIES[[i]])
			pp$HONER10[[i]] <- honer2(pp$DBH10[[i]],pp$HT10[[i]],species = pp$SPECIES[[i]]);
		}
	}
}

##############################################################################
######		Fit to power function and use for small DBHs		######
##############################################################################

### Make a working dataset for BECO with DBHs and BVs from all years in one column,
### but use each tagged individual only once
beco <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK86) | ### 86 trees
	pp$SPECIES=="BECO" & !is.na(pp$CLARK87) | ### 87 trees
	pp$SPECIES=="BECO" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87) | ### 98 trees
	pp$SPECIES=="BECO" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK86),]$CLARK86
ht86 <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK86),]$HT86
elevs <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK86),]$ELEV
plots <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK86),]$PLOT
canht <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK86),]$CANHT86
year <- rep(86,length(bv86))
beco86 <- data.frame(plots,ht86,bv86,canht,elevs,year)
names(beco86) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK87),]$CLARK87
ht87 <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK87),]$HT87
elevs <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK87),]$PLOT
canht <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK87),]$CANHT87
year <- rep(87,length(bv87))
beco87 <- data.frame(plots,ht87,bv87,canht,elevs,year)
names(beco87) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CLARK98
ht98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$HT98
elevs <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$PLOT
canht <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CANHT98
year <- rep(98,length(bv98))
beco98 <- data.frame(plots,ht98,bv98,canht,elevs,year)
names(beco98) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CLARK10
ht10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$HT10
elevs <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$ELEV
plots <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$PLOT
canht <- pp[pp$SPECIES=="BECO" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CANHT10
year <- rep(10,length(bv10))
beco10 <- data.frame(plots,ht10,bv10,canht,elevs,year)
names(beco10) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

beco <- rbind(beco86,beco87)
beco <- rbind(beco,beco98)
beco <- rbind(beco,beco10)
beco <- beco[beco$PLOT > 3,]

### nls fit beco
becobv <- beco$BV
becoht <- beco$HT
beco.mod <- nls(becobv~a*becoht^b, start=list(a=.1,b=-.2))
plot(beco$HT,beco$BV)
curve(coef(beco.mod)[[1]]*x^coef(beco.mod)[[2]],add=T,col="red")

##########################################################################
#########					BEAL					########
#########										########
##########################################################################
beal <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK86) | ### 86 trees
	pp$SPECIES=="BEAL" & !is.na(pp$CLARK87) | ### 87 trees
	pp$SPECIES=="BEAL" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87) | ### 98 trees
	pp$SPECIES=="BEAL" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK86),]$CLARK86
ht86 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK86),]$HT86
elevs <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK86),]$ELEV
plots <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK86),]$PLOT
canht <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK86),]$CANHT86
year <- rep(86,length(bv86))
beal86 <- data.frame(plots,ht86,bv86,canht,elevs,year)
names(beal86) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK87),]$CLARK87
ht87 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK87),]$HT87
elevs <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK87),]$PLOT
canht <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK87),]$CANHT87
year <- rep(87,length(bv87))
beal87 <- data.frame(plots,ht87,bv87,canht,elevs,year)
names(beal87) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CLARK98
ht98 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$HT98
elevs <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$PLOT
canht <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CANHT98
year <- rep(98,length(bv98))
beal98 <- data.frame(plots,ht98,bv98,canht,elevs,year)
names(beal98) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CLARK10
ht10 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$HT10
elevs <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$ELEV
plots <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$PLOT
canht <- pp[pp$SPECIES=="BEAL" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CANHT10
year <- rep(10,length(bv10))
beal10 <- data.frame(plots,ht10,bv10,canht,elevs,year)
names(beal10) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

beal <- rbind(beal86,beal87)
beal <- rbind(beal,beal98)
beal <- rbind(beal,beal10)
beal <- beal[beal$PLOT > 3,]

### nls fit beal
bealbv <- beal$BV
bealht <- beal$HT
beal.mod <- nls(bealbv~a*bealht^b, start=list(a=.1,b=-.2))
plot(beal$HT,beal$BV)
curve(coef(beal.mod)[[1]]*x^coef(beal.mod)[[2]],add=T,col="red")

##########################################################################
#########					PRPE					########
#########										########
##########################################################################
prpe <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK86) | ### 86 trees
	pp$SPECIES=="PRPE" & !is.na(pp$CLARK87) | ### 87 trees
	pp$SPECIES=="PRPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87) | ### 98 trees
	pp$SPECIES=="PRPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK86),]$CLARK86
ht86 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK86),]$HT86
elevs <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK86),]$ELEV
plots <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK86),]$PLOT
canht <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK86),]$CANHT86
year <- rep(86,length(bv86))
prpe86 <- data.frame(plots,ht86,bv86,canht,elevs,year)
names(prpe86) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK87),]$CLARK87
ht87 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK87),]$HT87
elevs <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK87),]$PLOT
canht <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK87),]$CANHT87
year <- rep(87,length(bv87))
prpe87 <- data.frame(plots,ht87,bv87,canht,elevs,year)
names(prpe87) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CLARK98
ht98 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$HT98
elevs <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$PLOT
canht <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CANHT98
year <- rep(98,length(bv98))
prpe98 <- data.frame(plots,ht98,bv98,canht,elevs,year)
names(prpe98) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CLARK10
ht10 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$HT10
elevs <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$ELEV
plots <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$PLOT
canht <- pp[pp$SPECIES=="PRPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CANHT10
year <- rep(10,length(bv10))
prpe10 <- data.frame(plots,ht10,bv10,canht,elevs,year)
names(prpe10) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

prpe <- rbind(prpe86,prpe87)
prpe <- rbind(prpe,prpe98)
prpe <- rbind(prpe,prpe10)
prpe <- prpe[prpe$PLOT > 3,]

### nls fit prpe
prpebv <- prpe$BV
prpeht <- prpe$HT
prpe.mod <- nls(prpebv~a*prpeht^b, start=list(a=.1,b=-.2))
plot(prpe$HT,prpe$BV)
curve(coef(prpe.mod)[[1]]*x^coef(prpe.mod)[[2]],add=T,col="red")

##########################################################################
#########					ACPE					########
#########										########
##########################################################################
acpe <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK86) | ### 86 trees
	pp$SPECIES=="ACPE" & !is.na(pp$CLARK87) | ### 87 trees
	pp$SPECIES=="ACPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87) | ### 98 trees
	pp$SPECIES=="ACPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK86),]$CLARK86
ht86 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK86),]$HT86
elevs <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK86),]$ELEV
plots <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK86),]$PLOT
canht <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK86),]$CANHT86
year <- rep(86,length(bv86))
acpe86 <- data.frame(plots,ht86,bv86,canht,elevs,year)
names(acpe86) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK87),]$CLARK87
ht87 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK87),]$HT87
elevs <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK87),]$PLOT
canht <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK87),]$CANHT87
year <- rep(87,length(bv87))
acpe87 <- data.frame(plots,ht87,bv87,canht,elevs,year)
names(acpe87) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CLARK98
ht98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$HT98
elevs <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$PLOT
canht <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CANHT98
year <- rep(98,length(bv98))
acpe98 <- data.frame(plots,ht98,bv98,canht,elevs,year)
names(acpe98) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CLARK10
ht10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$HT10
elevs <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$ELEV
plots <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$PLOT
canht <- pp[pp$SPECIES=="ACPE" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CANHT10
year <- rep(10,length(bv10))
acpe10 <- data.frame(plots,ht10,bv10,canht,elevs,year)
names(acpe10) <- c("PLOT","HT","BV","CANHT","ELEV","YEAR")

acpe <- rbind(acpe86,acpe87)
acpe <- rbind(acpe,acpe98)
acpe <- rbind(acpe,acpe10)
acpe <- acpe[acpe$PLOT > 3,]

### nls fit acpe
acpebv <- acpe$BV
acpeht <- acpe$HT
acpe.mod <- nls(acpebv~a*acpeht^b, start=list(a=.1,b=-.2))
plot(acpe$HT,acpe$BV)
curve(coef(acpe.mod)[[1]]*x^coef(acpe.mod)[[2]],add=T,col="red")

##########################################################################
#########					ACSA					########
#########										########
##########################################################################
acsa <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK86) | ### 86 trees
	pp$SPECIES=="ACSA" & !is.na(pp$CLARK87) | ### 87 trees
	pp$SPECIES=="ACSA" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87) | ### 98 trees
	pp$SPECIES=="ACSA" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK86),]$CLARK86
ht86 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK86),]$HT86
elevs <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK86),]$ELEV
plots <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK86),]$PLOT
dbh <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK86),]$DBH86
year <- rep(86,length(bv86))
acsa86 <- data.frame(plots,ht86,bv86,dbh,elevs,year)
names(acsa86) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK87),]$CLARK87
ht87 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK87),]$HT87
elevs <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK87),]$PLOT
dbh <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK87),]$DBH87
year <- rep(87,length(bv87))
acsa87 <- data.frame(plots,ht87,bv87,dbh,elevs,year)
names(acsa87) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CLARK98
ht98 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$HT98
elevs <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$PLOT
dbh <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$DBH98
year <- rep(98,length(bv98))
acsa98 <- data.frame(plots,ht98,bv98,dbh,elevs,year)
names(acsa98) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CLARK10
ht10 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$HT10
elevs <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$ELEV
plots <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$PLOT
dbh <- pp[pp$SPECIES=="ACSA" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$DBH10
year <- rep(10,length(bv10))
acsa10 <- data.frame(plots,ht10,bv10,dbh,elevs,year)
names(acsa10) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

acsa <- rbind(acsa86,acsa87)
acsa <- rbind(acsa,acsa98)
acsa <- rbind(acsa,acsa10)
acsa <- acsa[acsa$PLOT > 3,]

#### nls isnt able to make a fit with the largest value included, which is much much bigger than the rest
acsa <- acsa[acsa$BV != max(acsa$BV),]
### nls fit acsa
acsabv <- acsa$BV
acsaht <- acsa$HT
acsa.mod <- nls(acsabv~a*acsaht^b, start=list(a=.1,b=-.2))
plot(acsa$HT,acsa$BV)
curve(coef(acsa.mod)[[1]]*x^coef(acsa.mod)[[2]],add=T,col="red")

##########################################################################
#########					ACSP					########
#########										########
##########################################################################
acsp <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK86) | ### 86 trees
	pp$SPECIES=="ACSP" & !is.na(pp$CLARK87) | ### 87 trees
	pp$SPECIES=="ACSP" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87) | ### 98 trees
	pp$SPECIES=="ACSP" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK86),]$CLARK86
ht86 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK86),]$HT86
elevs <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK86),]$ELEV
plots <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK86),]$PLOT
dbh <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK86),]$DBH86
year <- rep(86,length(bv86))
acsp86 <- data.frame(plots,ht86,bv86,dbh,elevs,year)
names(acsp86) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK87),]$CLARK87
ht87 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK87),]$HT87
elevs <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK87),]$PLOT
dbh <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK87),]$DBH87
year <- rep(87,length(bv87))
acsp87 <- data.frame(plots,ht87,bv87,dbh,elevs,year)
names(acsp87) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CLARK98
ht98 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$HT98
elevs <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$PLOT
dbh <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$DBH98
year <- rep(98,length(bv98))
acsp98 <- data.frame(plots,ht98,bv98,dbh,elevs,year)
names(acsp98) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CLARK10
ht10 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$HT10
elevs <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$ELEV
plots <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$PLOT
dbh <- pp[pp$SPECIES=="ACSP" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$DBH10
year <- rep(10,length(bv10))
acsp10 <- data.frame(plots,ht10,bv10,dbh,elevs,year)
names(acsp10) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

acsp <- rbind(acsp86,acsp87)
acsp <- rbind(acsp,acsp98)
acsp <- rbind(acsp,acsp10)
acsp <- acsp[acsp$PLOT > 3,]

#### nls isnt able to make a fit with the largest value included, which is much much bigger than the rest
acsp <- acsp[acsp$BV != max(acsp$BV),]
### nls fit acsp
acspbv <- acsp$BV
acspht <- acsp$HT
acsp.mod <- nls(acspbv~a*acspht^b, start=list(a=.1,b=-.2))
plot(acsp$HT,acsp$BV)
curve(coef(acsp.mod)[[1]]*x^coef(acsp.mod)[[2]],add=T,col="red")


##########################################################################
#########					SOAM					########
#########										########
##########################################################################
soam <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK86) | ### 86 trees
	pp$SPECIES=="SOAM" & !is.na(pp$CLARK87) | ### 87 trees
	pp$SPECIES=="SOAM" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87) | ### 98 trees
	pp$SPECIES=="SOAM" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),] ### 2010 trees

bv86 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK86),]$CLARK86
ht86 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK86),]$HT86
elevs <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK86),]$ELEV
plots <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK86),]$PLOT
dbh <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK86),]$DBH86
year <- rep(86,length(bv86))
soam86 <- data.frame(plots,ht86,bv86,dbh,elevs,year)
names(soam86) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv87 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK87),]$CLARK87
ht87 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK87),]$HT87
elevs <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK87),]$PLOT
dbh <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK87),]$DBH87
year <- rep(87,length(bv87))
soam87 <- data.frame(plots,ht87,bv87,dbh,elevs,year)
names(soam87) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv98 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$CLARK98
ht98 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$HT98
elevs <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$ELEV
plots <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$PLOT
dbh <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK98) & is.na(pp$CLARK86) & is.na(pp$CLARK87),]$DBH98
year <- rep(98,length(bv98))
soam98 <- data.frame(plots,ht98,bv98,dbh,elevs,year)
names(soam98) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

bv10 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$CLARK10
ht10 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$HT10
elevs <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$ELEV
plots <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$PLOT
dbh <- pp[pp$SPECIES=="SOAM" & !is.na(pp$CLARK10) & is.na(pp$CLARK86) & is.na(pp$CLARK87) & is.na(pp$CLARK98),]$DBH10
year <- rep(10,length(bv10))
soam10 <- data.frame(plots,ht10,bv10,dbh,elevs,year)
names(soam10) <- c("PLOT","HT","BV","DBH","ELEV","YEAR")

soam <- rbind(soam86,soam87)
soam <- rbind(soam,soam98)
soam <- rbind(soam,soam10)
soam <- soam[soam$PLOT > 3,]

### nls fit soam
soambv <- soam$BV
soamht <- soam$HT
soam.mod <- nls(soambv~a*soamht^b, start=list(a=.1,b=-.2))
plot(soam$HT,soam$BV)
curve(coef(soam.mod)[[1]]*x^coef(soam.mod)[[2]],add=T,col="red")

##############################################################################
######	Use fits to estimate BV of small trees (no ABBA/PIRU)		######
##############################################################################

for(i in 1:nrow(pp)) {
 	if(pp$SPECIES[[i]]=="BECO") {
		if(!is.na(pp$DBH86[[i]]) & !is.na(pp$HT86[[i]]) & is.na(pp$CLARK86[[i]])) {
			pp$CLARK86[[i]] <- (coef(beco.mod)[[1]]*pp$HT86[[i]]^coef(beco.mod)[[2]]);
		}
		if(!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]]) & is.na(pp$CLARK87[[i]])) {
			pp$CLARK87[[i]] <- (coef(beco.mod)[[1]]*pp$HT87[[i]]^coef(beco.mod)[[2]]);
		}
		if(!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]]) & is.na(pp$CLARK98[[i]])) {
			pp$CLARK98[[i]] <- (coef(beco.mod)[[1]]*pp$HT98[[i]]^coef(beco.mod)[[2]]);
		}
		if(!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]]) & is.na(pp$CLARK10[[i]])) {
			pp$CLARK10[[i]] <- (coef(beco.mod)[[1]]*pp$HT10[[i]]^coef(beco.mod)[[2]]);
		}
	}
 	if(pp$SPECIES[[i]]=="BEAL") {
		if(!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]]) & is.na(pp$CLARK86[[i]])) {
			pp$CLARK86[[i]] <- coef(beal.mod)[[1]]*pp$HT86[[i]]^coef(beal.mod)[[2]];
		}
		if(!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]]) & is.na(pp$CLARK87[[i]])) {
			pp$CLARK87[[i]] <- coef(beal.mod)[[1]]*pp$HT87[[i]]^coef(beal.mod)[[2]];
		}
		if(!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]]) & is.na(pp$CLARK98[[i]])) {
			pp$CLARK98[[i]] <- coef(beal.mod)[[1]]*pp$HT98[[i]]^coef(beal.mod)[[2]];
		}
		if(!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]]) & is.na(pp$CLARK10[[i]])) {
			pp$CLARK10[[i]] <- coef(beal.mod)[[1]]*pp$HT10[[i]]^coef(beal.mod)[[2]];
		}
	}
 	if(pp$SPECIES[[i]]=="SOAM") {
		if(!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]]) & is.na(pp$CLARK86[[i]])) {
			pp$CLARK86[[i]] <- coef(soam.mod)[[1]]*pp$HT86[[i]]^coef(soam.mod)[[2]];
		}
		if(!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]]) & is.na(pp$CLARK87[[i]])) {
			pp$CLARK87[[i]] <- coef(soam.mod)[[1]]*pp$HT87[[i]]^coef(soam.mod)[[2]];
		}
		if(!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]]) & is.na(pp$CLARK98[[i]])) {
			pp$CLARK98[[i]] <- coef(soam.mod)[[1]]*pp$HT98[[i]]^coef(soam.mod)[[2]];
		}
		if(!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]]) & is.na(pp$CLARK10[[i]])) {
			pp$CLARK10[[i]] <- coef(soam.mod)[[1]]*pp$HT10[[i]]^coef(soam.mod)[[2]];
		}
	}
 	if(pp$SPECIES[[i]]=="PRPE") {
		if(!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]]) & is.na(pp$CLARK86[[i]])) {
			pp$CLARK86[[i]] <- coef(prpe.mod)[[1]]*pp$HT86[[i]]^coef(prpe.mod)[[2]];
		}
		if(!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]]) & is.na(pp$CLARK87[[i]])) {
			pp$CLARK87[[i]] <- coef(prpe.mod)[[1]]*pp$HT87[[i]]^coef(prpe.mod)[[2]];
		}
		if(!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]]) & is.na(pp$CLARK98[[i]])) {
			pp$CLARK98[[i]] <- coef(prpe.mod)[[1]]*pp$HT98[[i]]^coef(prpe.mod)[[2]];
		}
		if(!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]]) & is.na(pp$CLARK10[[i]])) {
			pp$CLARK10[[i]] <- coef(prpe.mod)[[1]]*pp$HT10[[i]]^coef(prpe.mod)[[2]];
		}
	}
 	if(pp$SPECIES[[i]]=="ACSA") {
		if(!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]]) & is.na(pp$CLARK86[[i]])) {
			pp$CLARK86[[i]] <- coef(acsa.mod)[[1]]*pp$HT86[[i]]^coef(acsa.mod)[[2]];
		}
		if(!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]]) & is.na(pp$CLARK87[[i]])) {
			pp$CLARK87[[i]] <- coef(acsa.mod)[[1]]*pp$HT87[[i]]^coef(acsa.mod)[[2]];
		}
		if(!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]]) & is.na(pp$CLARK98[[i]])) {
			pp$CLARK98[[i]] <- coef(acsa.mod)[[1]]*pp$HT98[[i]]^coef(acsa.mod)[[2]];
		}
		if(!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]]) & is.na(pp$CLARK10[[i]])) {
			pp$CLARK10[[i]] <- coef(acsa.mod)[[1]]*pp$HT10[[i]]^coef(acsa.mod)[[2]];
		}
	}
 	if(pp$SPECIES[[i]]=="ACPE") {
		if(!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]]) & is.na(pp$CLARK86[[i]])) {
			pp$CLARK86[[i]] <- coef(acpe.mod)[[1]]*pp$HT86[[i]]^coef(acpe.mod)[[2]]
		}
		if(!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]]) & is.na(pp$CLARK87[[i]])) {
			pp$CLARK87[[i]] <- coef(acpe.mod)[[1]]*pp$HT87[[i]]^coef(acpe.mod)[[2]]
		}
		if(!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]]) & is.na(pp$CLARK98[[i]])) {
			pp$CLARK98[[i]] <- coef(acpe.mod)[[1]]*pp$HT98[[i]]^coef(acpe.mod)[[2]]
		}
		if(!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]]) & is.na(pp$CLARK10[[i]])) {
			pp$CLARK10[[i]] <- coef(acpe.mod)[[1]]*pp$HT10[[i]]^coef(acpe.mod)[[2]]
		}
	}
 	if(pp$SPECIES[[i]]=="ACSP") {
		if(!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]]) & is.na(pp$CLARK86[[i]])) {
			pp$CLARK86[[i]] <- coef(acsp.mod)[[1]]*pp$HT86[[i]]^coef(acsp.mod)[[2]]
		}
		if(!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]]) & is.na(pp$CLARK87[[i]])) {
			pp$CLARK87[[i]] <- coef(acsp.mod)[[1]]*pp$HT87[[i]]^coef(acsp.mod)[[2]]
		}
		if(!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]]) & is.na(pp$CLARK98[[i]])) {
			pp$CLARK98[[i]] <- coef(acsp.mod)[[1]]*pp$HT98[[i]]^coef(acsp.mod)[[2]]
		}
		if(!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]]) & is.na(pp$CLARK10[[i]])) {
			pp$CLARK10[[i]] <- coef(acsp.mod)[[1]]*pp$HT10[[i]]^coef(acsp.mod)[[2]]
		}
	}
 	if(pp$SPECIES[[i]]=="FAGR") {
		if(!is.na(pp$DBH86[[i]])&!is.na(pp$HT86[[i]])) {
			pp$CLARK86[[i]] <- honer2(pp$DBH86[[i]],pp$HT86[[i]],"FAGR")
		}
		if(!is.na(pp$DBH87[[i]])&!is.na(pp$HT87[[i]])) {
			pp$CLARK87[[i]] <- honer2(pp$DBH87[[i]],pp$HT87[[i]],"FAGR")
		}
		if(!is.na(pp$DBH98[[i]])&!is.na(pp$HT98[[i]])) {
			pp$CLARK98[[i]] <- honer2(pp$DBH98[[i]],pp$HT98[[i]],"FAGR")
		}
		if(!is.na(pp$DBH10[[i]])&!is.na(pp$HT10[[i]])) {
			pp$CLARK10[[i]] <- honer2(pp$DBH10[[i]],pp$HT10[[i]],"FAGR")
		}
	}
}


###check clarks
nrow(pp[!is.na(pp$HT10) & is.na(pp$CLARK10) & pp$SPECIES!="ABBA" & pp$SPECIES!= "PIRU",]) ### 1 --  BEPA
nrow(pp[!is.na(pp$HT98) & is.na(pp$CLARK98) & pp$SPECIES!="ABBA" & pp$SPECIES!= "PIRU",]) ### 13 --  10 BEPA and a few missing data
nrow(pp[!is.na(pp$HT87) & is.na(pp$CLARK87) & pp$SPECIES!="ABBA" & pp$SPECIES!= "PIRU",]) ### 16 these have heights in 87 but no dbhs
nrow(pp[!is.na(pp$HT86) & is.na(pp$CLARK86) & pp$SPECIES!="ABBA" & pp$SPECIES!= "PIRU",]) ### 24, BEPAs, unid, pyde, PRVI

### check kozaks
nrow(pp[!is.na(pp$HT10) & is.na(pp$KOZAK10) & pp$SPECIES=="ABBA" |
	!is.na(pp$HT10) & is.na(pp$KOZAK10) & pp$SPECIES== "PIRU",]) ### 2 --
nrow(pp[!is.na(pp$HT98) & is.na(pp$KOZAK98) & pp$SPECIES=="ABBA" |
	!is.na(pp$HT98) & is.na(pp$KOZAK98) & pp$SPECIES== "PIRU",]) ### 2 -
nrow(pp[!is.na(pp$HT87) & is.na(pp$KOZAK87) & pp$SPECIES=="ABBA" |
	!is.na(pp$HT87) & is.na(pp$KOZAK87) & pp$SPECIES== "PIRU",]) ### 53 - they all have heights in 87 but no DBHs, mostly from plot 14 subplot 2
nrow(pp[!is.na(pp$HT86) & is.na(pp$KOZAK86) & pp$SPECIES=="ABBA" |
	!is.na(pp$HT86) & is.na(pp$KOZAK86) & pp$SPECIES== "PIRU",]) ### 0

### Join Clarks and Kozaks columns for unified bole volume column, BV
pp$BV86 <- rep(NA, nrow(pp))
pp$BV87 <- rep(NA, nrow(pp))
pp$BV98 <- rep(NA, nrow(pp))
pp$BV10 <- rep(NA, nrow(pp))

for(i in 1:nrow(pp)) {
	if(!is.na(pp$CLARK86[[i]])) pp$BV86[[i]] <- pp$CLARK86[[i]]
	if(!is.na(pp$KOZAK86[[i]])) pp$BV86[[i]] <- pp$KOZAK86[[i]]
	if(!is.na(pp$CLARK87[[i]])) pp$BV87[[i]] <- pp$CLARK87[[i]]
	if(!is.na(pp$KOZAK87[[i]])) pp$BV87[[i]] <- pp$KOZAK87[[i]]
	if(!is.na(pp$CLARK98[[i]])) pp$BV98[[i]] <- pp$CLARK98[[i]]
	if(!is.na(pp$KOZAK98[[i]])) pp$BV98[[i]] <- pp$KOZAK98[[i]]
	if(!is.na(pp$CLARK10[[i]])) pp$BV10[[i]] <- pp$CLARK10[[i]]
	if(!is.na(pp$KOZAK10[[i]])) pp$BV10[[i]] <- pp$KOZAK10[[i]]
}

nrow(pp[!is.na(pp$HT10) & pp$CLARK10!=pp$BV10 & pp$SPECIES!="ABBA" & pp$SPECIES!="PIRU",]) ### 2 --

## Write data
write.csv(pp, "~/work/data/data/boles/boles.csv", row.names = FALSE)



