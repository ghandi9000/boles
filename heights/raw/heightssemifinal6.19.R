### 6/19 Species height equations

pp <- read.csv("C:\\Users\\David\\Dropbox\\Shared\\Datasets\\pptr.csv",header=T,sep=",")
### fix species names to all capitals
for(i in 1:nrow(pp)) {
	if(pp$SPECIES[[i]]=="piru") pp$SPECIES[[i]] <- "PIRU"
	if(pp$SPECIES[[i]]=="beal") pp$SPECIES[[i]] <- "BEAL"
	if(pp$SPECIES[[i]]=="abba") pp$SPECIES[[i]] <- "ABBA"
	if(pp$SPECIES[[i]]=="prpe") pp$SPECIES[[i]] <- "PRPE"
	if(pp$SPECIES[[i]]=="beco") pp$SPECIES[[i]] <- "BECO"
}
########################### FAGR ###################################################
fagr <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="FAGR" & !is.na(pp$HT87) & !is.na(pp$DBH87) & is.na(pp$HT86) | ### 87 trees
	pp$SPECIES=="FAGR" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87) | ### 98 trees
	pp$SPECIES=="FAGR" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
fagr <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(fagr) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT87) & is.na(pp$HT86),]$DBH98
ht98 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT87) & is.na(pp$HT86),]$HT98
elev98 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT87) & is.na(pp$HT86),]$ELEV
plots98 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT87) & is.na(pp$HT86),]$PLOT
canht98 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT87) & is.na(pp$HT86),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="FAGR" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

fagr <- rbind(fagr,data87)
fagr <- rbind(fagr,data98)
fagr <- rbind(fagr,data10)
fagr <- fagr[fagr$PLOT > 3,]
fagr <- fagr[fagr$HT > 1.37 & fagr$DBH > 0 | fagr$DBH == 0 & fagr$HT <= 1.37,]

fagrlowht <- fagr[fagr$ELEV=="L",]$HT
fagrlowdbh <- fagr[fagr$ELEV=="L",]$DBH
fagrlow.mod <- nls(fagrlowht~a*fagrlowdbh^b, start=list(a=1,b=0.5))

################################# PRPE ############################################
prpe <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="PRPE" & !is.na(pp$HT87) & !is.na(pp$DBH87) & is.na(pp$HT86) | ### 87 trees
	pp$SPECIES=="PRPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87) | ### 98 trees
	pp$SPECIES=="PRPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
prpe <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(prpe) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$DBH98
ht98 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$HT98
elev98 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$ELEV
plots98 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$PLOT
canht98 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98) & is.na(pp$HT87),]$DBH10
ht10 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98) & is.na(pp$HT87),]$HT10
elev10 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98) & is.na(pp$HT87),]$ELEV
plots10 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98) & is.na(pp$HT87),]$PLOT
canht10 <- pp[pp$SPECIES=="PRPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98) & is.na(pp$HT87),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

prpe <- rbind(prpe,data87)
prpe <- rbind(prpe,data98)
prpe <- rbind(prpe,data10)
prpe <- prpe[prpe$PLOT > 3,]
prpe <- prpe[prpe$HT > 1.37 & prpe$DBH > 0 | prpe$DBH == 0 & prpe$HT <= 1.37,]

prpelowht <- prpe[prpe$ELEV=="L",]$HT
prpelowdbh <- prpe[prpe$ELEV=="L",]$DBH
prpelow.mod <- nls(prpelowht~a*prpelowdbh^b, start=list(a=1,b=1))

################################# ACSP ############################################
acsp <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="ACSP" & !is.na(pp$HT87) & !is.na(pp$DBH87) & is.na(pp$HT86) | ### 87 trees
	pp$SPECIES=="ACSP" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87)| ### 98 trees
	pp$SPECIES=="ACSP" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
acsp <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(acsp) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$DBH98
ht98 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$HT98
elev98 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$ELEV
plots98 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$PLOT
canht98 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="ACSP" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

acsp <- rbind(acsp,data87)
acsp <- rbind(acsp,data98)
acsp <- rbind(acsp,data10)
acsp <- acsp[acsp$PLOT > 3,]
acsp <- acsp[acsp$HT > 1.37 & acsp$DBH > 0 | acsp$DBH == 0 & acsp$HT <= 1.37,]

acsplowht <- acsp[acsp$ELEV=="L",]$HT
acsplowdbh <- acsp[acsp$ELEV=="L",]$DBH
acsplow.mod <- nls(acsplowht~a*acsplowdbh^b, start=list(a=1,b=1))

################################# ACPE ############################################
acpe <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87) & is.na(pp$HT86) | ### 87 trees
	pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87) | ### 98 trees
	pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
acpe <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(acpe) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$DBH98
ht98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$HT98
elev98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$ELEV
plots98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$PLOT
canht98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

acpe <- rbind(acpe,data87)
acpe <- rbind(acpe,data98)
acpe <- rbind(acpe,data10)
acpe <- acpe[acpe$PLOT > 3,]
acpe <- acpe[acpe$HT > 1.37 & acpe$DBH > 0 | acpe$DBH == 0 & acpe$HT <= 1.37,]

acpelowht <- acpe[acpe$ELEV=="L",]$HT
acpelowdbh <- acpe[acpe$ELEV=="L",]$DBH
acpelow.mod <- nls(acpelowht~a*acpelowdbh^b, start=list(a=1,b=1))

################################# ACSA ############################################
acsa <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="ACSA" & !is.na(pp$HT87) & !is.na(pp$DBH87) | ### 87 trees
	pp$SPECIES=="ACSA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) | ### 98 trees
	pp$SPECIES=="ACSA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
acsa <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(acsa) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$DBH98
ht98 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$HT98
elev98 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$ELEV
plots98 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$PLOT
canht98 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="ACSA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

acsa <- rbind(acsa,data87)
acsa <- rbind(acsa,data98)
acsa <- rbind(acsa,data10)
acsa <- acsa[acsa$HT > 1.37 & acsa$DBH > 0 | acsa$DBH == 0 & acsa$HT <= 1.37,]

acsalowht <- acsa[acsa$ELEV=="L",]$HT
acsalowdbh <- acsa[acsa$ELEV=="L",]$DBH
acsalow.mod <- nls(acsalowht~a*acsalowdbh^b, start=list(a=1,b=1))

######################### BECO #######################################
beco <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="BECO" & !is.na(pp$HT87) & !is.na(pp$DBH87) | ### 87 trees
	pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87) | ### 98 trees
	pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),] ### 2010 trees

dbh86 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
beco <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(beco) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$DBH98
ht98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$HT98
elev98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$ELEV
plots98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$PLOT
canht98 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="BECO" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

beco <- rbind(beco,data87)
beco <- rbind(beco,data98)
beco <- rbind(beco,data10)
beco <- beco[beco$PLOT > 3,]
beco <- beco[beco$HT > 1.37 & beco$DBH > 0 | beco$DBH == 0 & beco$HT <= 1.37,]

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
beal <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="BEAL" & !is.na(pp$HT87) & !is.na(pp$DBH87) & is.na(pp$HT86) | ### 87 trees
	pp$SPECIES=="BEAL" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87) | ### 98 trees
	pp$SPECIES=="BEAL" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
beal <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(beal) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$DBH98
ht98 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$HT98
elev98 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$ELEV
plots98 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$PLOT
canht98 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86)  & is.na(pp$HT87) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="BEAL" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

beal <- rbind(beal,data87)
beal <- rbind(beal,data98)
beal <- rbind(beal,data10)
beal <- beal[beal$PLOT > 3,]
beal <- beal[beal$HT > 1.37 & beal$DBH > 0 | beal$DBH == 0 & beal$HT <= 1.37,]

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
piru <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="PIRU" & !is.na(pp$HT87) & !is.na(pp$DBH87) | ### 87 trees
	pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) | ### 98 trees
	pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),] ### 2010 trees

### sample sizes for elevation:: H-132, M-341, L-112
### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
piru <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(piru) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$DBH98
ht98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$HT98
elev98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$ELEV
plots98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$PLOT
canht98 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="PIRU" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

piru <- rbind(piru,data87)
piru <- rbind(piru,data98)
piru <- rbind(piru,data10)
piru <- piru[piru$HT > 1.37 & piru$DBH > 0 | piru$DBH == 0 & piru$HT <= 1.37,]

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
abba <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="ABBA" & !is.na(pp$HT87) & !is.na(pp$DBH87) | ### 87 trees
	pp$SPECIES=="ABBA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) | ### 98 trees
	pp$SPECIES=="ABBA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
abba <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(abba) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$DBH98
ht98 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$HT98
elev98 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$ELEV
plots98 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$PLOT
canht98 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="ABBA" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

abba <- rbind(abba,data87)
abba <- rbind(abba,data98)
abba <- rbind(abba,data10)
abba <- abba[abba$HT > 1.37 & abba$DBH > 0 | abba$DBH == 0 & abba$HT <= 1.37,]

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
soam <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="SOAM" & !is.na(pp$HT87) & !is.na(pp$DBH87) & is.na(pp$HT86) | ### 87 trees
	pp$SPECIES=="SOAM" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) | ### 98 trees
	pp$SPECIES=="SOAM" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT86
year <- rep(86,length(ht86))
soam <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(soam) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT87
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$DBH98
ht98 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$HT98
elev98 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$ELEV
plots98 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$PLOT
canht98 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) & is.na(pp$HT87),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="SOAM" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT87) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

soam <- rbind(soam,data87)
soam <- rbind(soam,data98)
soam <- rbind(soam,data10)
soam <- soam[soam$PLOT > 3,]
soam <- soam[soam$HT > 1.37 & soam$DBH > 0 | soam$DBH == 0 & soam$HT <= 1.37,]

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
			pp$HT86[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH86[[i]]^coef(fagrlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH86[[i]]^coef(fagrlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH86[[i]]^coef(fagrlow.mod)[[2]])
		}
	}
	##### PRPE #######
	if(pp$SPECIES[[i]]=="PRPE"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH86[[i]]^coef(prpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH86[[i]]^coef(prpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH86[[i]]^coef(prpelow.mod)[[2]])
		}
	}
	##### ACSP #######
	if(pp$SPECIES[[i]]=="ACSP"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH86[[i]]^coef(acsplow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH86[[i]]^coef(acsplow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH86[[i]]^coef(acsplow.mod)[[2]])
		}
	}
	##### ACSA #######
	if(pp$SPECIES[[i]]=="ACSA"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH86[[i]]^coef(acsalow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH86[[i]]^coef(acsalow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH86[[i]]^coef(acsalow.mod)[[2]])
		}
	}
	##### ACPE #######
	if(pp$SPECIES[[i]]=="ACPE"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH86[[i]]^coef(acpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH86[[i]]^coef(acpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH86[[i]]^coef(acpelow.mod)[[2]])
		}
	}
	##### ABBA #######
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(abbalowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(abbalowbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(abbalowabove.mod)[[1]] * pp$DBH86[[i]]^coef(abbalowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(abbamidbelow.mod)[[1]] * pp$DBH86[[i]]^coef(abbamidbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(abbamidabove.mod)[[1]] * pp$DBH86[[i]]^coef(abbamidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(abbahighbelow.mod)[[1]] * pp$DBH86[[i]]^coef(abbahighbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(abbahighabove.mod)[[1]] * pp$DBH86[[i]]^coef(abbahighabove.mod)[[2]])
		}
	}
	##### SOAM #######
	if(pp$SPECIES[[i]]=="SOAM"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT86[[i]] <- (coef(soamlow.mod)[[1]] * pp$DBH86[[i]]^coef(soamlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(soammid.mod)[[1]] * pp$DBH86[[i]]^coef(soammid.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(soamhigh.mod)[[1]] * pp$DBH86[[i]]^coef(soamhigh.mod)[[2]])
		}
	}
	##### PIRU #######
	if(pp$SPECIES[[i]]=="PIRU"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(pirulowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(pirulowbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(pirulowabove.mod)[[1]] * pp$DBH86[[i]]^coef(pirulowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(pirumidbelow.mod)[[1]] * pp$DBH86[[i]]^coef(pirumidbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(pirumidabove.mod)[[1]] * pp$DBH86[[i]]^coef(pirumidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(piruhighbelow.mod)[[1]] * pp$DBH86[[i]]^coef(piruhighbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(piruhighabove.mod)[[1]] * pp$DBH86[[i]]^coef(piruhighabove.mod)[[2]])
		}
	}
	##### BEAL #######
	if(pp$SPECIES[[i]]=="BEAL"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(beallowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(beallowbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(beallowabove.mod)[[1]] * pp$DBH86[[i]]^coef(beallowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT86[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH86[[i]]^coef(bealmid.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT86[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH86[[i]]^coef(bealmid.mod)[[2]])
		}
	}
	##### BECO #######
	if(pp$SPECIES[[i]]=="BECO"&is.na(pp$HT86[[i]]) & !is.na(pp$DBH86[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH86[[i]])) {
		pp$HTPRED86[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(becolowbelow.mod)[[1]] * pp$DBH86[[i]]^coef(becolowbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(becolowabove.mod)[[1]] * pp$DBH86[[i]]^coef(becolowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(becomidbelow.mod)[[1]] * pp$DBH86[[i]]^coef(becomidbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(becomidabove.mod)[[1]] * pp$DBH86[[i]]^coef(becomidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH86[[i]]<pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(becohighbelow.mod)[[1]] * pp$DBH86[[i]]^coef(becohighbelow.mod)[[2]])
			if(pp$DBH86[[i]]>=pp$CANDBH86[[i]]) pp$HT86[[i]] <- (coef(becohighabove.mod)[[1]] * pp$DBH86[[i]]^coef(becohighabove.mod)[[2]])
		}
	}
}
######################################## 1987 ############################################
for(i in 1:nrow(pp)) {
	##### FAGR #######
	if(pp$SPECIES[[i]]=="FAGR"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH87[[i]]^coef(fagrlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH87[[i]]^coef(fagrlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH87[[i]]^coef(fagrlow.mod)[[2]])
		}
	}
	##### PRPE #######
	if(pp$SPECIES[[i]]=="PRPE"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH87[[i]]^coef(prpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH87[[i]]^coef(prpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH87[[i]]^coef(prpelow.mod)[[2]])
		}
	}
	##### ACSP #######
	if(pp$SPECIES[[i]]=="ACSP"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH87[[i]]^coef(acsplow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH87[[i]]^coef(acsplow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH87[[i]]^coef(acsplow.mod)[[2]])
		}
	}
	##### ACSA #######
	if(pp$SPECIES[[i]]=="ACSA"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH87[[i]]^coef(acsalow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH87[[i]]^coef(acsalow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH87[[i]]^coef(acsalow.mod)[[2]])
		}
	}
	##### ACPE #######
	if(pp$SPECIES[[i]]=="ACPE"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH87[[i]]^coef(acpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH87[[i]]^coef(acpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH87[[i]]^coef(acpelow.mod)[[2]])
		}
	}
	##### ABBA #######
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(abbalowbelow.mod)[[1]] * pp$DBH87[[i]]^coef(abbalowbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(abbalowabove.mod)[[1]] * pp$DBH87[[i]]^coef(abbalowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(abbamidbelow.mod)[[1]] * pp$DBH87[[i]]^coef(abbamidbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(abbamidabove.mod)[[1]] * pp$DBH87[[i]]^coef(abbamidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(abbahighbelow.mod)[[1]] * pp$DBH87[[i]]^coef(abbahighbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(abbahighabove.mod)[[1]] * pp$DBH87[[i]]^coef(abbahighabove.mod)[[2]])
		}
	}
	##### SOAM #######
	if(pp$SPECIES[[i]]=="SOAM"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT87[[i]] <- (coef(soamlow.mod)[[1]] * pp$DBH87[[i]]^coef(soamlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(soammid.mod)[[1]] * pp$DBH87[[i]]^coef(soammid.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(soamhigh.mod)[[1]] * pp$DBH87[[i]]^coef(soamhigh.mod)[[2]])
		}
	}
	##### PIRU #######
	if(pp$SPECIES[[i]]=="PIRU"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(pirulowbelow.mod)[[1]] * pp$DBH87[[i]]^coef(pirulowbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(pirulowabove.mod)[[1]] * pp$DBH87[[i]]^coef(pirulowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(pirumidbelow.mod)[[1]] * pp$DBH87[[i]]^coef(pirumidbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(pirumidabove.mod)[[1]] * pp$DBH87[[i]]^coef(pirumidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(piruhighbelow.mod)[[1]] * pp$DBH87[[i]]^coef(piruhighbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(piruhighabove.mod)[[1]] * pp$DBH87[[i]]^coef(piruhighabove.mod)[[2]])
		}
	}
	##### BEAL #######
	if(pp$SPECIES[[i]]=="BEAL"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(beallowbelow.mod)[[1]] * pp$DBH87[[i]]^coef(beallowbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(beallowabove.mod)[[1]] * pp$DBH87[[i]]^coef(beallowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT87[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH87[[i]]^coef(bealmid.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT87[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH87[[i]]^coef(bealmid.mod)[[2]])
		}
	}
	##### BECO #######
	if(pp$SPECIES[[i]]=="BECO"&is.na(pp$HT87[[i]]) & !is.na(pp$DBH87[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH87[[i]])) {
		pp$HTPRED87[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(becolowbelow.mod)[[1]] * pp$DBH87[[i]]^coef(becolowbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(becolowabove.mod)[[1]] * pp$DBH87[[i]]^coef(becolowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(becomidbelow.mod)[[1]] * pp$DBH87[[i]]^coef(becomidbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(becomidabove.mod)[[1]] * pp$DBH87[[i]]^coef(becomidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH87[[i]]<pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(becohighbelow.mod)[[1]] * pp$DBH87[[i]]^coef(becohighbelow.mod)[[2]])
			if(pp$DBH87[[i]]>=pp$CANDBH87[[i]]) pp$HT87[[i]] <- (coef(becohighabove.mod)[[1]] * pp$DBH87[[i]]^coef(becohighabove.mod)[[2]])
		}
	}
}
######################################## 1998 ############################################
for(i in 1:nrow(pp)) {
	##### FAGR #######
	if(pp$SPECIES[[i]]=="FAGR"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH98[[i]]^coef(fagrlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH98[[i]]^coef(fagrlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH98[[i]]^coef(fagrlow.mod)[[2]])
		}
	}
	##### PRPE #######
	if(pp$SPECIES[[i]]=="PRPE"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH98[[i]]^coef(prpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH98[[i]]^coef(prpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH98[[i]]^coef(prpelow.mod)[[2]])
		}
	}
	##### ACSP #######
	if(pp$SPECIES[[i]]=="ACSP"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH98[[i]]^coef(acsplow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH98[[i]]^coef(acsplow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH98[[i]]^coef(acsplow.mod)[[2]])
		}
	}
	##### ACSA #######
	if(pp$SPECIES[[i]]=="ACSA"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH98[[i]]^coef(acsalow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH98[[i]]^coef(acsalow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH98[[i]]^coef(acsalow.mod)[[2]])
		}
	}
	##### ACPE #######
	if(pp$SPECIES[[i]]=="ACPE"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH98[[i]]^coef(acpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH98[[i]]^coef(acpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH98[[i]]^coef(acpelow.mod)[[2]])
		}
	}
	##### ABBA #######
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(abbalowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(abbalowbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(abbalowabove.mod)[[1]] * pp$DBH98[[i]]^coef(abbalowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(abbamidbelow.mod)[[1]] * pp$DBH98[[i]]^coef(abbamidbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(abbamidabove.mod)[[1]] * pp$DBH98[[i]]^coef(abbamidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(abbahighbelow.mod)[[1]] * pp$DBH98[[i]]^coef(abbahighbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(abbahighabove.mod)[[1]] * pp$DBH98[[i]]^coef(abbahighabove.mod)[[2]])
		}
	}
	##### SOAM #######
	if(pp$SPECIES[[i]]=="SOAM"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT98[[i]] <- (coef(soamlow.mod)[[1]] * pp$DBH98[[i]]^coef(soamlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(soammid.mod)[[1]] * pp$DBH98[[i]]^coef(soammid.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(soamhigh.mod)[[1]] * pp$DBH98[[i]]^coef(soamhigh.mod)[[2]])
		}
	}
	##### PIRU #######
	if(pp$SPECIES[[i]]=="PIRU"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(pirulowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(pirulowbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(pirulowabove.mod)[[1]] * pp$DBH98[[i]]^coef(pirulowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(pirumidbelow.mod)[[1]] * pp$DBH98[[i]]^coef(pirumidbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(pirumidabove.mod)[[1]] * pp$DBH98[[i]]^coef(pirumidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(piruhighbelow.mod)[[1]] * pp$DBH98[[i]]^coef(piruhighbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(piruhighabove.mod)[[1]] * pp$DBH98[[i]]^coef(piruhighabove.mod)[[2]])
		}
	}
	##### BEAL #######
	if(pp$SPECIES[[i]]=="BEAL"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(beallowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(beallowbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(beallowabove.mod)[[1]] * pp$DBH98[[i]]^coef(beallowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT98[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH98[[i]]^coef(bealmid.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT98[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH98[[i]]^coef(bealmid.mod)[[2]])
		}
	}
	##### BECO #######
	if(pp$SPECIES[[i]]=="BECO"&is.na(pp$HT98[[i]]) & !is.na(pp$DBH98[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH98[[i]])) {
		pp$HTPRED98[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(becolowbelow.mod)[[1]] * pp$DBH98[[i]]^coef(becolowbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(becolowabove.mod)[[1]] * pp$DBH98[[i]]^coef(becolowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(becomidbelow.mod)[[1]] * pp$DBH98[[i]]^coef(becomidbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(becomidabove.mod)[[1]] * pp$DBH98[[i]]^coef(becomidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH98[[i]]<pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(becohighbelow.mod)[[1]] * pp$DBH98[[i]]^coef(becohighbelow.mod)[[2]])
			if(pp$DBH98[[i]]>=pp$CANDBH98[[i]]) pp$HT98[[i]] <- (coef(becohighabove.mod)[[1]] * pp$DBH98[[i]]^coef(becohighabove.mod)[[2]])
		}
	}
}
######################################## 2010 ############################################
for(i in 1:nrow(pp)) {	
	##### FAGR #######
	if(pp$SPECIES[[i]]=="FAGR"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH10[[i]]^coef(fagrlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH10[[i]]^coef(fagrlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(fagrlow.mod)[[1]] * pp$DBH10[[i]]^coef(fagrlow.mod)[[2]])
		}
	}
	##### PRPE #######
	if(pp$SPECIES[[i]]=="PRPE"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH10[[i]]^coef(prpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH10[[i]]^coef(prpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(prpelow.mod)[[1]] * pp$DBH10[[i]]^coef(prpelow.mod)[[2]])
		}
	}
	##### ACSP #######
	if(pp$SPECIES[[i]]=="ACSP"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH10[[i]]^coef(acsplow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH10[[i]]^coef(acsplow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(acsplow.mod)[[1]] * pp$DBH10[[i]]^coef(acsplow.mod)[[2]])
		}
	}
	##### ACSA #######
	if(pp$SPECIES[[i]]=="ACSA"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH10[[i]]^coef(acsalow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH10[[i]]^coef(acsalow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(acsalow.mod)[[1]] * pp$DBH10[[i]]^coef(acsalow.mod)[[2]])
		}
	}
	##### ACPE #######
	if(pp$SPECIES[[i]]=="ACPE"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH10[[i]]^coef(acpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH10[[i]]^coef(acpelow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(acpelow.mod)[[1]] * pp$DBH10[[i]]^coef(acpelow.mod)[[2]])
		}
	}
	##### ABBA #######
	if(pp$SPECIES[[i]]=="ABBA"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(abbalowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(abbalowbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(abbalowabove.mod)[[1]] * pp$DBH10[[i]]^coef(abbalowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(abbamidbelow.mod)[[1]] * pp$DBH10[[i]]^coef(abbamidbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(abbamidabove.mod)[[1]] * pp$DBH10[[i]]^coef(abbamidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(abbahighbelow.mod)[[1]] * pp$DBH10[[i]]^coef(abbahighbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(abbahighabove.mod)[[1]] * pp$DBH10[[i]]^coef(abbahighabove.mod)[[2]])
		}
	}
	##### SOAM #######
	if(pp$SPECIES[[i]]=="SOAM"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			pp$HT10[[i]] <- (coef(soamlow.mod)[[1]] * pp$DBH10[[i]]^coef(soamlow.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(soammid.mod)[[1]] * pp$DBH10[[i]]^coef(soammid.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(soamhigh.mod)[[1]] * pp$DBH10[[i]]^coef(soamhigh.mod)[[2]])
		}
	}
	##### PIRU #######
	if(pp$SPECIES[[i]]=="PIRU"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(pirulowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(pirulowbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(pirulowabove.mod)[[1]] * pp$DBH10[[i]]^coef(pirulowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(pirumidbelow.mod)[[1]] * pp$DBH10[[i]]^coef(pirumidbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(pirumidabove.mod)[[1]] * pp$DBH10[[i]]^coef(pirumidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(piruhighbelow.mod)[[1]] * pp$DBH10[[i]]^coef(piruhighbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(piruhighabove.mod)[[1]] * pp$DBH10[[i]]^coef(piruhighabove.mod)[[2]])
		}
	}
	##### BEAL #######
	if(pp$SPECIES[[i]]=="BEAL"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(beallowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(beallowbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(beallowabove.mod)[[1]] * pp$DBH10[[i]]^coef(beallowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			pp$HT10[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH10[[i]]^coef(bealmid.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			pp$HT10[[i]] <- (coef(bealmid.mod)[[1]] * pp$DBH10[[i]]^coef(bealmid.mod)[[2]])
		}
	}
	##### BECO #######
	if(pp$SPECIES[[i]]=="BECO"&is.na(pp$HT10[[i]]) & !is.na(pp$DBH10[[i]]) & pp$ELEV[[i]]!="" & !is.na(pp$CANDBH10[[i]])) {
		pp$HTPRED10[[i]] <- 1
		if(pp$ELEV[[i]] == "L") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(becolowbelow.mod)[[1]] * pp$DBH10[[i]]^coef(becolowbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(becolowabove.mod)[[1]] * pp$DBH10[[i]]^coef(becolowabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "M") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(becomidbelow.mod)[[1]] * pp$DBH10[[i]]^coef(becomidbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(becomidabove.mod)[[1]] * pp$DBH10[[i]]^coef(becomidabove.mod)[[2]])
		}
		if(pp$ELEV[[i]] == "H") {
			if(pp$DBH10[[i]]<pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(becohighbelow.mod)[[1]] * pp$DBH10[[i]]^coef(becohighbelow.mod)[[2]])
			if(pp$DBH10[[i]]>=pp$CANDBH10[[i]]) pp$HT10[[i]] <- (coef(becohighabove.mod)[[1]] * pp$DBH10[[i]]^coef(becohighabove.mod)[[2]])
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


write.csv(pp,"C:/Users/Noah/Desktop/ppnew.csv")











