### 7/6
### bole volumes using equations from clark et al. 1991
### equations were designed for southern species

### input units are dbh in cm, height in m
### output is in m^3

### clark volume equation

clark <- function(dbh,height,species) {
	##### Species Parameters ########################
	if(species=="PRPE" | species=="SOAM") a1 = 0.92487; b1 = -0.89867;  r = 37.12714; c = 0.48776; e = 1.50579;
		p = 6.18866; b = 1.64261; a = 0.55071;
###	if(species=="ABBA" | species=="PIRU") a1 = 0.92487; b1 = -0.89867;  r = 31.66250; c = 0.57402; e = 110.96;
###		p = 8.573; b = 2.36238; a = 0.68464  ### using the coefficients for loblolly pine to get comparisons for ABBA and PIRU

	if(species=="ACPE" | species=="ACSA" | species=="ACSP") a1 = 0.93991; b1 = -1.62226; r = 22.00135; c = 0.45472;
		e = 166.1; p = 7.31546; b = 1.17064; a = 0.27213;
	if(species=="FAGR") a1 = 0.91141; b1 = -0.6673; r = 44.36826; c = 1.22158; e = 79.44636;
		p = 6.36236; b = 1.11382; a = 0.14312;
	if(species=="BEAL" | species=="BECO" | species=="BEPA") a1 = 0.85516; b1 = -0.00134; r = 49.41385; c = 1.01241;
		e = -91.82769; p = 11.23179; b = 1.19704; a = 0.23928;
	###if(species=="SOAM") a1 = 0.91531; b1 = -0.96788; r = 0.64600; c = 0.49680; e = 127.87;
		p = 7.52915; b = 1.49287; a = 0.47222

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



clark(16/0.3937,90/3.28,species="ABBA")



