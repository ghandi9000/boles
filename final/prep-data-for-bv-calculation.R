## Prepares the data for input into bolevolumefinal.R
## 1) Add canopy heights
## 2) Add canopy dbhs
library(plyr)
dat <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10_updatedNov2013.csv")
canhts <- read.csv("~/work/data/data/boles/canhts.csv")
candbhs <- read.csv("~/work/data/data/boles/candbhs.csv")

yrs <- c(86, 87, 98, 10)
output <- ddply(dat, .(PPLOT), .fun = function(x) {
    x <- droplevels(x);
    for (yr in yrs) {
        canht <- canhts[unique(x$PPLOT) == canhts$PPLOT, paste0("CANHT",yr)]
        candbh <- candbhs[unique(x$PPLOT) == candbhs$PPLOT, paste0("CANDBH",yr)]
        x[,paste0("CANHT",yr)] <- rep(canht, nrow(x))
        x[,paste0("CANDBH",yr)] <- rep(candbh, nrow(x))
    }
    x
})

## Change column names:
## 1) HTTCR -> HT
## 2) SPEC -> SPECIES
## 3) ELEVCL -> ELEV
## 4) PPLOT -> PLOT
names(output) <- gsub("HTTCR", "HT", names(output))
names(output) <- gsub("SPEC", "SPECIES", names(output))
names(output) <- gsub("ELEVCL", "ELEV", names(output))
names(output) <- gsub("PPLOT", "PLOT", names(output))

## write data
write.csv(output, "~/work/data/data/boles/prepped-for-bole-calculation.csv",
          row.names = FALSE)
