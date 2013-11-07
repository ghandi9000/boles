## Check bole volume calculations
## dat <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10_updatedNov2013.csv")
pp <- read.csv("~/work/data/data/boles/boles.csv")
prep <- read.csv("~/work/data/data/boles/prepped-for-bole-calculation.csv")

## trees with dbh and no height in 1998
## nrow(subset(dat, STAT98 == "ALIVE" & !is.na(DBH98) & is.na(HTTCR98)))
## nrow(subset(prep, STAT98 == "ALIVE" & !is.na(DBH98) & is.na(HT98)))
print("Number of live trees with DBH missing HT in 1998: ",
      nrow(subset(pp, STAT98 == "ALIVE" & !is.na(DBH98) & is.na(HT98))))

print("Number of live trees with HT missing BV in 1998: ",
      nrow(subset(pp, STAT98 == "ALIVE" & !is.na(HT98) & is.na(BV98))))


