### Bole Volume Calculations
### First uses the Kozak/Clark/Honer equations to calculate the bole volume of large trees,
### then extrapolate to small trees using standard power function
source("~/work/functions/functions-boles.R")
pp <- read.csv("~/work/data/data/boles/prepped-for-bole-calculation.csv")

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



