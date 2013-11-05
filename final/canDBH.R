## Calculate mean canopy DBHs:
## Mean canopy DBH is the mean of trees >= canopy height in each plot / year
library(plyr)
dat <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10_updatedNov2013.csv")
canhts <- read.csv("~/work/boles/canopyHeight/canhts.csv")

## calculate canopy dbhs
yrs <- c(86, 87, 98, 10)

candbhs <- ddply(dat, .(PPLOT), .fun = function(x) {
    x <- droplevels(x)
    canDBH <- sapply(yrs, function(y) {
        yr <- y
        canHt <- canhts[canhts$PPLOT==unique(x$PPLOT),paste0("CANHT",yr)]
        stat <- x[, paste0("STAT",yr)]
        ifelse (unique(x$PPLOT) == 15 & yr == 86, ## use heights from 87 for plot 15
            { htCol <- x[, paste0("HTTCR",87)]; colName <- paste0("HTTCR",87) },
               { htCol <- x[, paste0("HTTCR", yr)]; colName <- paste0("HTTCR",yr) })
        pp <- x[!is.na(htCol) & stat == "ALIVE",]
        dbhs <- pp[which(pp[, colName] > canHt), paste0("DBH", yr)]
        mean(dbhs, na.rm = TRUE) ## calculate canopy heights
    })
})

## name and write
names(candbhs) <- c("PPLOT", paste0("CANDBH",yrs))
write.csv(candbhs, "~/work/boles/canopyHeight/candbhs.csv", row.names = FALSE)
