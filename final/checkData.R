## Check bole volume calculations
## dat <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10_updatedNov2013.csv")
pp <- read.csv("~/work/data/data/boles/boles.csv")
prep <- read.csv("~/work/data/data/boles/prepped-for-bole-calculation.csv")

## trees with dbh and no height in 1998
## nrow(subset(dat, STAT98 == "ALIVE" & !is.na(DBH98) & is.na(HTTCR98)))
## nrow(subset(prep, STAT98 == "ALIVE" & !is.na(DBH98) & is.na(HT98)))
yrs <- c(86, 87, 98, 10)
output <- sapply(yrs, function(yr) {
    stat = paste0("STAT",yr); ht = paste0("HT",yr); bv = paste0("BV",yr);
    dbh = paste0("DBH",yr);
    c(
        assign(paste0("noBV",yr), nrow(pp[pp[,stat]=="ALIVE" &
                                          !is.na(pp[,ht]) & is.na(pp[,bv]),])),
        assign(paste0("noHT",yr), nrow(pp[pp[,stat]=="ALIVE" &
                                          !is.na(pp[,dbh]) & is.na(pp[,ht]),]))
        )
})
colnames(output) <- paste0("yr", yrs)
rownames(output) <- c("Missing BV", "Missing HT")

print(output)

tst <- subset(pp, STAT98 == "ALIVE" & !is.na(HT98) & is.na(BV98))
print("Trees with HT98 but missing BV98 by species: ")
print(table(tst$SPECIES))
