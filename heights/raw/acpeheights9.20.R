### ACPE heights 6.20


################################# ACPE ############################################
acpe <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86) | ### 86 trees
	pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87) & is.na(pp$HT86) | ### 87 trees
	pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86) | ### 98 trees
	pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),] ### 2010 trees

### create new working dataset with dbh, ht, elev, and year the dbh and ht were collected
dbh86 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$DBH86
ht86 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$HT86
elevs <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$ELEV
plots <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$PLOT
canht <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT86) & !is.na(pp$DBH86),]$CANHT98
year <- rep(86,length(ht86))
acpe <- data.frame(plots,dbh86,ht86,canht,elevs,year)
names(acpe) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh87 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$DBH87
ht87 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$HT87
elevs <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$ELEV
plots <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$PLOT
canht <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT87) & !is.na(pp$DBH87),]$CANHT98
year <- rep(87,length(ht87))
data87 <- data.frame(plots,dbh87,ht87,canht,elevs,year)
names(data87) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$DBH98
ht98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$HT98
elev98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$ELEV
plots98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$PLOT
canht98 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT98) & !is.na(pp$DBH98) & is.na(pp$HT86),]$CANHT98
year98 <- rep(98,length(ht98))
data98 <- data.frame(plots98,dbh98,ht98,canht98,elev98,year98)
names(data98) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

dbh10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$DBH10
ht10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$HT10
elev10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$ELEV
plots10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$PLOT
canht10 <- pp[pp$SPECIES=="ACPE" & !is.na(pp$HT10) & !is.na(pp$DBH10) & is.na(pp$HT86) & is.na(pp$HT98),]$CANHT10
year10 <- rep(10,length(ht10))
data10 <- data.frame(plots10,dbh10,ht10,canht10,elev10,year10)
names(data10) <- c("PLOT","DBH","HT","CANHT","ELEV","YEAR")

acpe <- rbind(acpe,data87)
acpe <- rbind(acpe,data98)
acpe <- rbind(acpe,data10)
acpe <- acpe[acpe$PLOT > 3,]

acpelowht <- acpe[acpe$ELEV=="L",]$HT
acpelowdbh <- acpe[acpe$ELEV=="L",]$DBH
acpelow.mod <- nls(acpelowht~a*acpelowdbh^b, start=list(a=1,b=1))

plot(acpe$DBH,acpe$HT)
curve(coef(acpelow.mod)[[1]]*x^coef(acpelow.mod)[[2]],add=T)


