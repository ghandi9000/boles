### Predict heights for trees with DBH and no height,
## Subsets of data used vary across species (differing sample sizes)
pp <- read.csv("~/work/data/data/boles/prepped-for-bole-calculation.csv")

make.working.dataset <- function(spp) {
	trees.from.86 <- which ( 	pp$SPECIES == spp & !is.na(pp$HT86) & !is.na(pp$DBH86) & pp$DBH86 != 0 )
	trees.from.87 <- which ( 	pp$SPECIES == spp & !is.na(pp$HT87) & !is.na(pp$DBH87) & pp$DBH87 != 0 )
	trees.from.87 <- trees.from.87[is.na(match(trees.from.87,trees.from.86))]
	trees.from.98 <- which (	pp$SPECIES == spp & !is.na(pp$HT98) & !is.na(pp$DBH98) & pp$DBH98 != 0 )
	trees.from.98 <- trees.from.98[is.na(match(trees.from.98,trees.from.86))]
	trees.from.98 <- trees.from.98[is.na(match(trees.from.98,trees.from.87))]
	trees.from.10 <- which (	pp$SPECIES == spp & !is.na(pp$HT10) & !is.na(pp$DBH10) & pp$DBH10 != 0 )
	trees.from.10 <- trees.from.10[is.na(match(trees.from.10,trees.from.86))]
	trees.from.10 <- trees.from.10[is.na(match(trees.from.10,trees.from.87))]
	trees.from.10 <- trees.from.10[is.na(match(trees.from.10,trees.from.98))]
	dbh86 <- pp[trees.from.86,]$DBH86
	ht86 <- pp[trees.from.86,]$HT86
	elev86 <- pp[trees.from.86,]$ELEV
	plots86 <- pp[trees.from.86,]$PLOT
	canht86 <- pp[trees.from.86,]$CANHT86
	year86 <- rep(86,length(ht86))
	data86 <- data.frame(plots86,dbh86,ht86,canht86,elev86,year86)
	names(data86) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

	dbh87 <- pp[trees.from.87,]$DBH87
	ht87 <- pp[trees.from.87,]$HT87
	elev87 <- pp[trees.from.87,]$ELEV
	plots87 <- pp[trees.from.87,]$PLOT
	canht87 <- pp[trees.from.87,]$CANHT87
	year87 <- rep(87,length(ht87))
	data87 <- data.frame(plots87,dbh87,ht87,canht87,elev87,year87)
	names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

	dbh98 <- pp[trees.from.98,]$DBH98
	ht98 <- pp[trees.from.98,]$HT98
	elev98 <- pp[trees.from.98,]$ELEV
	plots98 <- pp[trees.from.98,]$PLOT
	canht98 <- pp[trees.from.98,]$CANHT98
	year98 <- rep(98,length(ht98))
	data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
	names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

	dbh10 <- pp[trees.from.10,]$DBH10
	ht10 <- pp[trees.from.10,]$HT10
	elev10 <- pp[trees.from.10,]$ELEV
	plots10 <- pp[trees.from.10,]$PLOT
	canht10 <- pp[trees.from.10,]$CANHT10
	year10 <- rep(10,length(ht10))
	data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
	names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

	dataset <- rbind(data86,data87,data98,data10)
	dataset <- subset(dataset, PLOT > 3)
}
########################### FAGR ###################################################
spp <- "FAGR"
fagr <- make.working.dataset(spp)

fagrlowht <- fagr[fagr$ELEV=="L",]$HT
fagrlowdbh <- fagr[fagr$ELEV=="L",]$DBH
fagrlow.mod <- nls(fagrlowht~a*fagrlowdbh^b, start=list(a=1,b=0.5))

################################# PRPE ############################################
spp <- "PRPE"
prpe <- make.working.dataset(spp)

prpelowht <- prpe[prpe$ELEV=="L",]$HT
prpelowdbh <- prpe[prpe$ELEV=="L",]$DBH
prpelow.mod <- nls(prpelowht~a*prpelowdbh^b, start=list(a=1,b=1))

################################# ACSP ############################################
spp <- "ACSP"
acsp <- make.working.dataset(spp)

acsplowht <- acsp[acsp$ELEV=="L",]$HT
acsplowdbh <- acsp[acsp$ELEV=="L",]$DBH
acsplow.mod <- nls(acsplowht~a*acsplowdbh^b, start=list(a=1,b=1))

################################# ACPE ############################################
spp <- "ACPE"
acpe <- make.working.dataset(spp)

acpelowht <- acpe[acpe$ELEV=="L",]$HT
acpelowdbh <- acpe[acpe$ELEV=="L",]$DBH
acpelow.mod <- nls(acpelowht~a*acpelowdbh^b, start=list(a=1,b=1))

################################# ACSA ############################################
spp <- "ACSA"
acsa <- make.working.dataset(spp)

acsalowht <- acsa[acsa$ELEV=="L",]$HT
acsalowdbh <- acsa[acsa$ELEV=="L",]$DBH
acsalow.mod <- nls(acsalowht~a*acsalowdbh^b, start=list(a=1,b=1))

######################### BECO #######################################
spp <- "BECO"
beco <- make.working.dataset(spp)
############# nonlinear fits above and below canopy ##########################
becolowbelowht <- beco[beco$ELEV=="L" & beco$HT<beco$CANHT,]$HT
becolowbelowdbh <- beco[beco$ELEV=="L" & beco$HT<beco$CANHT,]$DBH
becolowaboveht <- beco[beco$ELEV=="L" & beco$HT>=beco$CANHT,]$HT
becolowabovedbh <- beco[beco$ELEV=="L" & beco$HT>=beco$CANHT,]$DBH

becolowbelow.mod <- nls(becolowbelowht~a*becolowbelowdbh^b, start=list(a=1,b=1))
becolowabove.mod <- nls(becolowaboveht~a*becolowabovedbh^b, start=list(a=1,b=1))

becomidbelowht <- beco[beco$ELEV=="M" & beco$HT<beco$CANHT,]$HT
becomidbelowdbh <- beco[beco$ELEV=="M" & beco$HT<beco$CANHT,]$DBH
becomidaboveht <- beco[beco$ELEV=="M" & beco$HT>=beco$CANHT,]$HT
becomidabovedbh <- beco[beco$ELEV=="M" & beco$HT>=beco$CANHT,]$DBH

becomidbelow.mod <- nls(becomidbelowht~a*becomidbelowdbh^b, start=list(a=1,b=1))
becomidabove.mod <- nls(becomidaboveht~a*becomidabovedbh^b, start=list(a=1,b=1))

becohighbelowht <- beco[beco$ELEV=="H" & beco$HT<beco$CANHT,]$HT
becohighbelowdbh <- beco[beco$ELEV=="H" & beco$HT<beco$CANHT,]$DBH
becohighaboveht <- beco[beco$ELEV=="H" & beco$HT>=beco$CANHT,]$HT
becohighabovedbh <- beco[beco$ELEV=="H" & beco$HT>=beco$CANHT,]$DBH

becohighbelow.mod <- nls(becohighbelowht~a*becohighbelowdbh^b, start=list(a=1,b=1))
becohighabove.mod <- nls(becohighaboveht~a*becohighabovedbh^b, start=list(a=1,b=1))

################################# BEAL ############################################
spp <- "BEAL"
beal <- make.working.dataset(spp)
### Fits
beallowbelowht <- beal[beal$ELEV=="L"&beal$HT<beal$CANHT&beal$DBH<40,]$HT
beallowbelowdbh <- beal[beal$ELEV=="L"&beal$HT<beal$CANHT&beal$DBH<40,]$DBH
beallowaboveht <- beal[beal$ELEV=="L"&beal$HT >= beal$CANHT,]$HT
beallowabovedbh <- beal[beal$ELEV=="L"&beal$HT >= beal$CANHT,]$DBH
beallowbelow.mod <- nls(beallowbelowht~a*beallowbelowdbh^b, start=list(a=1,b=1))
beallowabove.mod <- nls(beallowaboveht~a*beallowabovedbh^b, start=list(a=1,b=1))

beallowht <- beal[beal$ELEV=="L",]$HT
beallowdbh <- beal[beal$ELEV=="L",]$DBH
beallow.mod <- nls(beallowht~a*beallowdbh^b, start=list(a=1,b=1))

bealmidht <- beal[beal$ELEV=="M",]$HT
bealmiddbh <- beal[beal$ELEV=="M",]$DBH
bealmid.mod <- nls(bealmidht~a*bealmiddbh^b, start=list(a=1,b=1))

################################# PIRU ############################################
spp <- "PIRU"
piru <- make.working.dataset(spp)
############# nonlinear fits above and below canopy ##########################
pirulowbelowht <- piru[piru$ELEV=="L" & piru$HT<piru$CANHT,]$HT
pirulowbelowdbh <- piru[piru$ELEV=="L" & piru$HT<piru$CANHT,]$DBH
pirulowaboveht <- piru[piru$ELEV=="L" & piru$HT>=piru$CANHT,]$HT
pirulowabovedbh <- piru[piru$ELEV=="L" & piru$HT>=piru$CANHT,]$DBH

pirulowbelow.mod <- nls(pirulowbelowht~a*pirulowbelowdbh^b, start=list(a=1,b=1))
pirulowabove.mod <- nls(pirulowaboveht~a*pirulowabovedbh^b, start=list(a=1,b=1))

pirumidbelowht <- piru[piru$ELEV=="M" & piru$HT<piru$CANHT,]$HT
pirumidbelowdbh <- piru[piru$ELEV=="M" & piru$HT<piru$CANHT,]$DBH
pirumidaboveht <- piru[piru$ELEV=="M" & piru$HT>=piru$CANHT,]$HT
pirumidabovedbh <- piru[piru$ELEV=="M" & piru$HT>=piru$CANHT,]$DBH

pirumidbelow.mod <- nls(pirumidbelowht~a*pirumidbelowdbh^b, start=list(a=1,b=1))
pirumidabove.mod <- nls(pirumidaboveht~a*pirumidabovedbh^b, start=list(a=1,b=1))

piruhighbelowht <- piru[piru$ELEV=="H" & piru$HT<piru$CANHT,]$HT
piruhighbelowdbh <- piru[piru$ELEV=="H" & piru$HT<piru$CANHT,]$DBH
piruhighaboveht <- piru[piru$ELEV=="H" & piru$HT>=piru$CANHT,]$HT
piruhighabovedbh <- piru[piru$ELEV=="H" & piru$HT>=piru$CANHT,]$DBH

piruhighbelow.mod <- nls(piruhighbelowht~a*piruhighbelowdbh^b, start=list(a=1,b=1))
piruhighabove.mod <- nls(piruhighaboveht~a*piruhighabovedbh^b, start=list(a=1,b=1))

#########################################################################################
#######										#######################
#######					ABBA					#######################
####### Using all yrs for fits, yrs separate is now in other file	#######################
#######										#######################
#######										#######################
#########################################################################################
spp <- "ABBA"
abba <- make.working.dataset(spp)
############# nonlinear fits above and below canopy ##########################
abbalowbelowht <- abba[abba$ELEV=="L" & abba$HT<abba$CANHT,]$HT
abbalowbelowdbh <- abba[abba$ELEV=="L" & abba$HT<abba$CANHT,]$DBH
abbalowaboveht <- abba[abba$ELEV=="L" & abba$HT>=abba$CANHT,]$HT
abbalowabovedbh <- abba[abba$ELEV=="L" & abba$HT>=abba$CANHT,]$DBH

abbalowbelow.mod <- nls(abbalowbelowht~a*abbalowbelowdbh^b, start=list(a=1,b=1))
abbalowabove.mod <- nls(abbalowaboveht~a*abbalowabovedbh^b, start=list(a=1,b=1))

abbamidbelowht <- abba[abba$ELEV=="M" & abba$HT<abba$CANHT,]$HT
abbamidbelowdbh <- abba[abba$ELEV=="M" & abba$HT<abba$CANHT,]$DBH
abbamidaboveht <- abba[abba$ELEV=="M" & abba$HT>=abba$CANHT,]$HT
abbamidabovedbh <- abba[abba$ELEV=="M" & abba$HT>=abba$CANHT,]$DBH

abbamidbelow.mod <- nls(abbamidbelowht~a*abbamidbelowdbh^b, start=list(a=1,b=1))
abbamidabove.mod <- nls(abbamidaboveht~a*abbamidabovedbh^b, start=list(a=1,b=1))

abbahighbelowht <- abba[abba$ELEV=="H" & abba$HT<abba$CANHT,]$HT
abbahighbelowdbh <- abba[abba$ELEV=="H" & abba$HT<abba$CANHT,]$DBH
abbahighaboveht <- abba[abba$ELEV=="H" & abba$HT>=abba$CANHT,]$HT
abbahighabovedbh <- abba[abba$ELEV=="H" & abba$HT>=abba$CANHT,]$DBH

abbahighbelow.mod <- nls(abbahighbelowht~a*abbahighbelowdbh^b, start=list(a=1,b=1))
abbahighabove.mod <- nls(abbahighaboveht~a*abbahighabovedbh^b, start=list(a=1,b=1))

################################# SOAM ############################################
spp <- "SOAM"
soam <- make.working.dataset(spp)
### Fits
soamlowht <- soam[soam$ELEV=="L",]$HT
soamlowdbh <- soam[soam$ELEV=="L",]$DBH
soamlow.mod <- nls(soamlowht~a*soamlowdbh^b, start=list(a=1,b=1))

soammidht <- soam[soam$ELEV=="M",]$HT
soammiddbh <- soam[soam$ELEV=="M",]$DBH
soammid.mod <- nls(soammidht~a*soammiddbh^b, start=list(a=1,b=1))

soamhighht <- soam[soam$ELEV=="H",]$HT
soamhighdbh <- soam[soam$ELEV=="H",]$DBH
soamhigh.mod <- nls(soamhighht~a*soamhighdbh^b, start=list(a=1,b=1))

################################### height prediction ####################################
##### columns to identify predicted values
pp$HTPRED86 <- rep(NA,nrow(pp)) ### column that will say whether the height was measured or predicted
pp$HTPRED87 <- rep(NA,nrow(pp)) ### column that will say whether the height was measured or predicted
pp$HTPRED98 <- rep(NA,nrow(pp)) ### column that will say whether the height was measured or predicted
pp$HTPRED10 <- rep(NA,nrow(pp)) ### column that will say whether the height was measured or predicted
######################################## 1986 ############################################
for(i in 1:nrow(pp)) {
	##### FAGR #######
	if(pp$SPECIES[[i]]=="FAGR"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH86[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH86[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH86[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
	}
	##### PRPE #######
	if(pp$SPECIES[[i]]=="PRPE"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH86[[i]]^coef(prpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH86[[i]]^coef(prpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH86[[i]]^coef(prpelow.mod)[[2]]); next;
		}
	}
	##### ACSP #######
	if(pp$SPECIES[[i]]=="ACSP"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH86[[i]]^coef(acsplow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH86[[i]]^coef(acsplow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH86[[i]]^coef(acsplow.mod)[[2]]); next;
		}
	}
	##### ACSA #######
	if(pp$SPECIES[[i]]=="ACSA"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH86[[i]]^coef(acsalow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH86[[i]]^coef(acsalow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH86[[i]]^coef(acsalow.mod)[[2]]); next;
		}
	}
	##### ACPE #######
	if(pp$SPECIES[[i]]=="ACPE"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH86[[i]]^coef(acpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH86[[i]]^coef(acpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH86[[i]]^coef(acpelow.mod)[[2]]); next;
		}
	}
	##### ABBA #######
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(abbalowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(abbalowbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(abbalowabove.mod)[[1]] * pp$DBH86[[i]]^coef(abbalowabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(abbamidbelow.mod)[[1]] * pp$DBH86[[i]]^coef(abbamidbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(abbamidabove.mod)[[1]] * pp$DBH86[[i]]^coef(abbamidabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(abbahighbelow.mod)[[1]] * pp$DBH86[[i]]^coef(abbahighbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(abbahighabove.mod)[[1]] * pp$DBH86[[i]]^coef(abbahighabove.mod)[[2]]); next; }		}
	}
	##### SOAM #######
	if(pp$SPECIES[[i]]=="SOAM"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(soamlow.mod)[[1]] * pp$DBH86[[i]]^coef(soamlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(soammid.mod)[[1]] * pp$DBH86[[i]]^coef(soammid.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(soamhigh.mod)[[1]] * pp$DBH86[[i]]^coef(soamhigh.mod)[[2]]); next;
		}
	}
	##### PIRU #######
	if(pp$SPECIES[[i]]=="PIRU"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(pirulowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(pirulowbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(pirulowabove.mod)[[1]] * pp$DBH86[[i]]^coef(pirulowabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(pirumidbelow.mod)[[1]] * pp$DBH86[[i]]^coef(pirumidbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(pirumidabove.mod)[[1]] * pp$DBH86[[i]]^coef(pirumidabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(piruhighbelow.mod)[[1]] * pp$DBH86[[i]]^coef(piruhighbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(piruhighabove.mod)[[1]] * pp$DBH86[[i]]^coef(piruhighabove.mod)[[2]]); next; }
		}
	}
	##### BEAL #######
	if(pp$SPECIES[[i]]=="BEAL"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(beallowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(beallowbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(beallowabove.mod)[[1]] * pp$DBH86[[i]]^coef(beallowabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH86[[i]]^coef(bealmid.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH86[[i]]^coef(bealmid.mod)[[2]]); next;
		}
	}
	##### BECO #######
	if(pp$SPECIES[[i]]=="BECO"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(becolowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(becolowbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(becolowabove.mod)[[1]] * pp$DBH86[[i]]^coef(becolowabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(becomidbelow.mod)[[1]] * pp$DBH86[[i]]^coef(becomidbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(becomidabove.mod)[[1]] * pp$DBH86[[i]]^coef(becomidabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(becohighbelow.mod)[[1]] * pp$DBH86[[i]]^coef(becohighbelow.mod)[[2]]); next; }
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) { pp$HT86[[i]] <- (coef(becohighabove.mod)[[1]] * pp$DBH86[[i]]^coef(becohighabove.mod)[[2]]); next; }
		}
	}
}
begin.time <- Sys.time()
######################################## 1987 ############################################
for(i in 1:nrow(pp)) {
	##### FAGR #######
	if(pp$SPECIES[[i]]=="FAGR"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH87[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH87[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH87[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
	}
	##### PRPE #######
	if(pp$SPECIES[[i]]=="PRPE"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH87[[i]]^coef(prpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH87[[i]]^coef(prpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH87[[i]]^coef(prpelow.mod)[[2]]); next;
		}
	}
	##### ACSP #######
	if(pp$SPECIES[[i]]=="ACSP"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH87[[i]]^coef(acsplow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH87[[i]]^coef(acsplow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH87[[i]]^coef(acsplow.mod)[[2]]); next;
		}
	}
	##### ACSA #######
	if(pp$SPECIES[[i]]=="ACSA"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH87[[i]]^coef(acsalow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH87[[i]]^coef(acsalow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH87[[i]]^coef(acsalow.mod)[[2]]); next;
		}
	}
	##### ACPE #######
	if(pp$SPECIES[[i]]=="ACPE"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH87[[i]]^coef(acpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH87[[i]]^coef(acpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH87[[i]]^coef(acpelow.mod)[[2]]); next;
		}
	}
	##### ABBA #######
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) { pp$HT87[[i]] <- (coef(abbalowbelow.mod)[[1]] * pp$DBH87[[i]]^coef(abbalowbelow.mod)[[2]]); next; }
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) { pp$HT87[[i]] <- (coef(abbalowabove.mod)[[1]] * pp$DBH87[[i]]^coef(abbalowabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) { pp$HT87[[i]] <- (coef(abbamidbelow.mod)[[1]] * pp$DBH87[[i]]^coef(abbamidbelow.mod)[[2]]); next; }
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) { pp$HT87[[i]] <- (coef(abbamidabove.mod)[[1]] * pp$DBH87[[i]]^coef(abbamidabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) { pp$HT87[[i]] <- (coef(abbahighbelow.mod)[[1]] * pp$DBH87[[i]]^coef(abbahighbelow.mod)[[2]]); next; }
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) { pp$HT87[[i]] <- (coef(abbahighabove.mod)[[1]] * pp$DBH87[[i]]^coef(abbahighabove.mod)[[2]]); next; }
		}
	}
	##### SOAM #######
	if(pp$SPECIES[[i]]=="SOAM"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(soamlow.mod)[[1]] * pp$DBH87[[i]]^coef(soamlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(soammid.mod)[[1]] * pp$DBH87[[i]]^coef(soammid.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(soamhigh.mod)[[1]] * pp$DBH87[[i]]^coef(soamhigh.mod)[[2]]); next;
		}
	}
	##### PIRU #######
	if(pp$SPECIES[[i]]=="PIRU"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(pirulowbelow.mod)[[1]] * pp$DBH87[[i]]^coef(pirulowbelow.mod)[[2]]); next;}
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(pirulowabove.mod)[[1]] * pp$DBH87[[i]]^coef(pirulowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(pirumidbelow.mod)[[1]] * pp$DBH87[[i]]^coef(pirumidbelow.mod)[[2]]); next;}
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(pirumidabove.mod)[[1]] * pp$DBH87[[i]]^coef(pirumidabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(piruhighbelow.mod)[[1]] * pp$DBH87[[i]]^coef(piruhighbelow.mod)[[2]]); next;}
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(piruhighabove.mod)[[1]] * pp$DBH87[[i]]^coef(piruhighabove.mod)[[2]]); next;}
		}
	}
	##### BEAL #######
	if(pp$SPECIES[[i]]=="BEAL"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(beallowbelow.mod)[[1]] * pp$DBH87[[i]]^coef(beallowbelow.mod)[[2]]); next;}
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(beallowabove.mod)[[1]] * pp$DBH87[[i]]^coef(beallowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH87[[i]]^coef(bealmid.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH87[[i]]^coef(bealmid.mod)[[2]]); next;
		}
	}
	##### BECO #######
	if(pp$SPECIES[[i]]=="BECO"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(becolowbelow.mod)[[1]] * pp$DBH87[[i]]^coef(becolowbelow.mod)[[2]]); next;}
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(becolowabove.mod)[[1]] * pp$DBH87[[i]]^coef(becolowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(becomidbelow.mod)[[1]] * pp$DBH87[[i]]^coef(becomidbelow.mod)[[2]]); next;}
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(becomidabove.mod)[[1]] * pp$DBH87[[i]]^coef(becomidabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(becohighbelow.mod)[[1]] * pp$DBH87[[i]]^coef(becohighbelow.mod)[[2]]); next;}
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) {pp$HT87[[i]] <- (coef(becohighabove.mod)[[1]] * pp$DBH87[[i]]^coef(becohighabove.mod)[[2]]); next;}
		}
	}
}
begin.time <- Sys.time()
######################################## 1998 ############################################
for(i in 1:nrow(pp)) {
	##### FAGR #######
	if(pp$SPECIES[[i]]=="FAGR"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH98[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH98[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH98[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
	}
	##### PRPE #######
	if(pp$SPECIES[[i]]=="PRPE"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH98[[i]]^coef(prpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH98[[i]]^coef(prpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH98[[i]]^coef(prpelow.mod)[[2]]); next;
		}
	}
	##### ACSP #######
	if(pp$SPECIES[[i]]=="ACSP"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH98[[i]]^coef(acsplow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH98[[i]]^coef(acsplow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH98[[i]]^coef(acsplow.mod)[[2]]); next;
		}
	}
	##### ACSA #######
	if(pp$SPECIES[[i]]=="ACSA"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH98[[i]]^coef(acsalow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH98[[i]]^coef(acsalow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH98[[i]]^coef(acsalow.mod)[[2]]); next;
		}
	}
	##### ACPE #######
	if(pp$SPECIES[[i]]=="ACPE"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH98[[i]]^coef(acpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH98[[i]]^coef(acpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH98[[i]]^coef(acpelow.mod)[[2]]); next;
		}
	}
	##### ABBA #######
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) { pp$HT98[[i]] <- (coef(abbalowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(abbalowbelow.mod)[[2]]); next; }
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) { pp$HT98[[i]] <- (coef(abbalowabove.mod)[[1]] * pp$DBH98[[i]]^coef(abbalowabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) { pp$HT98[[i]] <- (coef(abbamidbelow.mod)[[1]] * pp$DBH98[[i]]^coef(abbamidbelow.mod)[[2]]); next; }
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) { pp$HT98[[i]] <- (coef(abbamidabove.mod)[[1]] * pp$DBH98[[i]]^coef(abbamidabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) { pp$HT98[[i]] <- (coef(abbahighbelow.mod)[[1]] * pp$DBH98[[i]]^coef(abbahighbelow.mod)[[2]]); next; }
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) { pp$HT98[[i]] <- (coef(abbahighabove.mod)[[1]] * pp$DBH98[[i]]^coef(abbahighabove.mod)[[2]]); next; }
		}
	}
	##### SOAM #######
	if(pp$SPECIES[[i]]=="SOAM"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(soamlow.mod)[[1]] * pp$DBH98[[i]]^coef(soamlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(soammid.mod)[[1]] * pp$DBH98[[i]]^coef(soammid.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(soamhigh.mod)[[1]] * pp$DBH98[[i]]^coef(soamhigh.mod)[[2]]); next;
		}
	}
	##### PIRU #######
	if(pp$SPECIES[[i]]=="PIRU"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(pirulowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(pirulowbelow.mod)[[2]]); next;}
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(pirulowabove.mod)[[1]] * pp$DBH98[[i]]^coef(pirulowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(pirumidbelow.mod)[[1]] * pp$DBH98[[i]]^coef(pirumidbelow.mod)[[2]]); next;}
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(pirumidabove.mod)[[1]] * pp$DBH98[[i]]^coef(pirumidabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(piruhighbelow.mod)[[1]] * pp$DBH98[[i]]^coef(piruhighbelow.mod)[[2]]); next;}
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(piruhighabove.mod)[[1]] * pp$DBH98[[i]]^coef(piruhighabove.mod)[[2]]); next;}
		}
	}
	##### BEAL #######
	if(pp$SPECIES[[i]]=="BEAL"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(beallowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(beallowbelow.mod)[[2]]); next;}
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(beallowabove.mod)[[1]] * pp$DBH98[[i]]^coef(beallowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH98[[i]]^coef(bealmid.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH98[[i]]^coef(bealmid.mod)[[2]]); next;
		}
	}
	##### BECO #######
	if(pp$SPECIES[[i]]=="BECO"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(becolowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(becolowbelow.mod)[[2]]); next;}
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(becolowabove.mod)[[1]] * pp$DBH98[[i]]^coef(becolowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(becomidbelow.mod)[[1]] * pp$DBH98[[i]]^coef(becomidbelow.mod)[[2]]); next;}
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(becomidabove.mod)[[1]] * pp$DBH98[[i]]^coef(becomidabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(becohighbelow.mod)[[1]] * pp$DBH98[[i]]^coef(becohighbelow.mod)[[2]]); next;}
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) {pp$HT98[[i]] <- (coef(becohighabove.mod)[[1]] * pp$DBH98[[i]]^coef(becohighabove.mod)[[2]]); next;}
		}
	}
}
end.time <- Sys.time()

######################################## 2010 ############################################
for(i in 1:nrow(pp)) {
	##### FAGR #######
	if(pp$SPECIES[[i]]=="FAGR"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH10[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH10[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH10[[i]]^coef(fagrlow.mod)[[2]]); next;
		}
	}
	##### PRPE #######
	if(pp$SPECIES[[i]]=="PRPE"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH10[[i]]^coef(prpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH10[[i]]^coef(prpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH10[[i]]^coef(prpelow.mod)[[2]]); next;
		}
	}
	##### ACSP #######
	if(pp$SPECIES[[i]]=="ACSP"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH10[[i]]^coef(acsplow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH10[[i]]^coef(acsplow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH10[[i]]^coef(acsplow.mod)[[2]]); next;
		}
	}
	##### ACSA #######
	if(pp$SPECIES[[i]]=="ACSA"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH10[[i]]^coef(acsalow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH10[[i]]^coef(acsalow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH10[[i]]^coef(acsalow.mod)[[2]]); next;
		}
	}
	##### ACPE #######
	if(pp$SPECIES[[i]]=="ACPE"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH10[[i]]^coef(acpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH10[[i]]^coef(acpelow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH10[[i]]^coef(acpelow.mod)[[2]]); next;
		}
	}
	##### ABBA #######
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) { pp$HT10[[i]] <- (coef(abbalowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(abbalowbelow.mod)[[2]]); next; }
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) { pp$HT10[[i]] <- (coef(abbalowabove.mod)[[1]] * pp$DBH10[[i]]^coef(abbalowabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) { pp$HT10[[i]] <- (coef(abbamidbelow.mod)[[1]] * pp$DBH10[[i]]^coef(abbamidbelow.mod)[[2]]); next; }
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) { pp$HT10[[i]] <- (coef(abbamidabove.mod)[[1]] * pp$DBH10[[i]]^coef(abbamidabove.mod)[[2]]); next; }
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) { pp$HT10[[i]] <- (coef(abbahighbelow.mod)[[1]] * pp$DBH10[[i]]^coef(abbahighbelow.mod)[[2]]); next; }
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) { pp$HT10[[i]] <- (coef(abbahighabove.mod)[[1]] * pp$DBH10[[i]]^coef(abbahighabove.mod)[[2]]); next; }
		}
	}
	##### SOAM #######
	if(pp$SPECIES[[i]]=="SOAM"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(soamlow.mod)[[1]] * pp$DBH10[[i]]^coef(soamlow.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(soammid.mod)[[1]] * pp$DBH10[[i]]^coef(soammid.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(soamhigh.mod)[[1]] * pp$DBH10[[i]]^coef(soamhigh.mod)[[2]]); next;
		}
	}
	##### PIRU #######
	if(pp$SPECIES[[i]]=="PIRU"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(pirulowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(pirulowbelow.mod)[[2]]); next;}
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(pirulowabove.mod)[[1]] * pp$DBH10[[i]]^coef(pirulowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(pirumidbelow.mod)[[1]] * pp$DBH10[[i]]^coef(pirumidbelow.mod)[[2]]); next;}
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(pirumidabove.mod)[[1]] * pp$DBH10[[i]]^coef(pirumidabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(piruhighbelow.mod)[[1]] * pp$DBH10[[i]]^coef(piruhighbelow.mod)[[2]]); next;}
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(piruhighabove.mod)[[1]] * pp$DBH10[[i]]^coef(piruhighabove.mod)[[2]]); next;}
		}
	}
	##### BEAL #######
	if(pp$SPECIES[[i]]=="BEAL"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(beallowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(beallowbelow.mod)[[2]]); next;}
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(beallowabove.mod)[[1]] * pp$DBH10[[i]]^coef(beallowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH10[[i]]^coef(bealmid.mod)[[2]]); next;
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH10[[i]]^coef(bealmid.mod)[[2]]); next;
		}
	}
	##### BECO #######
	if(pp$SPECIES[[i]]=="BECO"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(becolowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(becolowbelow.mod)[[2]]); next;}
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(becolowabove.mod)[[1]] * pp$DBH10[[i]]^coef(becolowabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(becomidbelow.mod)[[1]] * pp$DBH10[[i]]^coef(becomidbelow.mod)[[2]]); next;}
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(becomidabove.mod)[[1]] * pp$DBH10[[i]]^coef(becomidabove.mod)[[2]]); next;}
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(becohighbelow.mod)[[1]] * pp$DBH10[[i]]^coef(becohighbelow.mod)[[2]]); next;}
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) {pp$HT10[[i]] <- (coef(becohighabove.mod)[[1]] * pp$DBH10[[i]]^coef(becohighabove.mod)[[2]]); next;}
		}
	}
}
windows()
par(mfrow = c(2,4))
### 1998
plot(pp[pp$SPECIES=="ABBA",]$DBH98,pp[pp$SPECIES=="ABBA",]$HT98)
points(pp[pp$SPECIES=="ABBA"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="ABBA"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="SOAM",]$DBH98,pp[pp$SPECIES=="SOAM",]$HT98)
points(pp[pp$SPECIES=="SOAM"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="SOAM"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="PIRU",]$DBH98,pp[pp$SPECIES=="PIRU",]$HT98)
points(pp[pp$SPECIES=="PIRU"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="PIRU"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="BECO",]$DBH98,pp[pp$SPECIES=="BECO",]$HT98)
points(pp[pp$SPECIES=="BECO"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="BECO"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)
### 2010
plot(pp[pp$SPECIES=="ABBA",]$DBH10,pp[pp$SPECIES=="ABBA",]$HT10)
points(pp[pp$SPECIES=="ABBA"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="ABBA"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="SOAM",]$DBH10,pp[pp$SPECIES=="SOAM",]$HT10)
points(pp[pp$SPECIES=="SOAM"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="SOAM"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="PIRU",]$DBH10,pp[pp$SPECIES=="PIRU",]$HT10)
points(pp[pp$SPECIES=="PIRU"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="PIRU"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="BECO",]$DBH10,pp[pp$SPECIES=="BECO",]$HT10)
points(pp[pp$SPECIES=="BECO"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="BECO"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

windows()
par(mfrow=c(2,5))
### 1998
plot(pp[pp$SPECIES=="ACSP",]$DBH98,pp[pp$SPECIES=="ACSP",]$HT98)
points(pp[pp$SPECIES=="ACSP"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="ACSP"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="ACPE",]$DBH98,pp[pp$SPECIES=="ACPE",]$HT98)
points(pp[pp$SPECIES=="ACPE"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="ACPE"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="FAGR",]$DBH98,pp[pp$SPECIES=="FAGR",]$HT98)
points(pp[pp$SPECIES=="FAGR"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="FAGR"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="BEAL",]$DBH98,pp[pp$SPECIES=="BEAL",]$HT98)
points(pp[pp$SPECIES=="BEAL"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="BEAL"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

plot(pp[pp$SPECIES=="PRPE",]$DBH98,pp[pp$SPECIES=="PRPE",]$HT98)
points(pp[pp$SPECIES=="PRPE"&pp$HTPRED98=="1",]$DBH98,pp[pp$SPECIES=="PRPE"&pp$HTPRED98=="1",]$HT98,col="red",lwd=3)

### 2010
plot(pp[pp$SPECIES=="ACSP",]$DBH10,pp[pp$SPECIES=="ACSP",]$HT10)
points(pp[pp$SPECIES=="ACSP"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="ACSP"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="ACPE",]$DBH10,pp[pp$SPECIES=="ACPE",]$HT10)
points(pp[pp$SPECIES=="ACPE"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="ACPE"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="FAGR",]$DBH10,pp[pp$SPECIES=="FAGR",]$HT10)
points(pp[pp$SPECIES=="FAGR"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="FAGR"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="BEAL",]$DBH10,pp[pp$SPECIES=="BEAL",]$HT10)
points(pp[pp$SPECIES=="BEAL"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="BEAL"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

plot(pp[pp$SPECIES=="PRPE",]$DBH10,pp[pp$SPECIES=="PRPE",]$HT10)
points(pp[pp$SPECIES=="PRPE"&pp$HTPRED10=="1",]$DBH10,pp[pp$SPECIES=="PRPE"&pp$HTPRED10=="1",]$HT10,col="red",lwd=3)

## write data
write.csv(pp,"~/work/data/data/boles/prepped-for-bole-calculation.csv", row.names=FALSE)

