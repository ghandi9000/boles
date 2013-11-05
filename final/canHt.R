## Calculate canopy heights by averaging height of codominant trees for each
##  permanent plot by year
## NOTES:
## Cautions:
## - 0 dbhs values that should be NA
##
## 1) There were no canopy positions for trees in 1986, so use positions from 1988
## 2) Plot 16 & 22 have no live codominant trees, so use 1998
## 3) Plot 15 use height from 1987 instead of 1986 (but everything else from 86)

## Master data is stored on dropbox
library(plyr)
dat <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10_updatedNov2013.csv")

yrs <- c(86, 87, 98, 10) ## yrs to calculate canopy heights

canhts <- ddply(dat, .(PPLOT), .fun = function(x) {
    x <- droplevels(x)
    nCodoms <- sapply(yrs, function(y) {
        yr <- y
        if (unique(x$PPLOT) == 16 && yr == 87 ||
            unique(x$PPLOT) == 22 && yr == 87) { yr <- 98 } ## use trees from 98 for plots 16&22
        stat <- x[, paste0("STAT",yr)]
        if(unique(x$PPLOT) == 15 && yr == 86) yr <- 87 ## use heights from 87
        htCol <- x[, paste0("HTTCR",yr)]
        pp <- x[!is.na(htCol) & stat == "ALIVE",]
        ifelse (y == 86, yy <- 88, yy <- yr) ## use can heights from 88 for 86 plots
        cpos <- paste0("CPOS",yy)
        codHeights <- pp[pp[,cpos] == "c", paste0("HTTCR",yr)]
        mean(codHeights) ## calculate canopy heights
    })
})

## name and write data
names(canhts) <- c("PPLOT", paste0("CANHT",yrs))
write.csv(canhts, "~/work/boles/canopyHeight/canhts.csv", row.names = FALSE)
## tst <- subset(dat, PPLOT == 13 & !is.na(HTTCR86) & STAT86 == "ALIVE" & CPOS88 == "c")
