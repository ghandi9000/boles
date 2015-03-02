## Script to take master data and recreate data with predicted heights,
##  bole volumes, canopy heights, and canopy dbhs
## Directories required (from repos):
## "~/work/boles/", "~/work/data/", "~/work/functions/"

## Master data
source("~/work/ecodatascripts/read/read-moose.R")
dat <- pp

## 1) Check to make sure that the data contains the necessary columns, report missing
##  if not present
yrs <- c(86, 87, 98, 10) # columns with year suffixes
cols <- c("PPLOT", paste0("DBH",yrs), paste0("STAT",yrs), paste0("HTTCR",yrs),
          paste0("CPOS",yrs))
except <- c("CPOS86") # remove CPOS86, we dont have that data
cols <- cols[!cols %in% except]
notThere <- cols[!cols %in% names(dat)]
if (length(present[!is.na(present)]) != length(cols)) { # report missing columns
    print("MISSING SOME REQUIRED COLUMNS IN MASTER DATA: ")
    print(c("Required columns: ", notThere))
}

## 2) get canopy heights and dbhs
## Creates: "~/work/data/data/boles/canhts.csv",
##          "~/work/data/data/boles/candbhs.csv"
source("~/work/boles/final/canHt.R")
source("~/work/boles/final/canDBH.R")

## 3) Add canopy height/dbh columns and change some column names
## Creates: "~/work/data/data/boles/prepped-for-bole-calculation.csv"
source("~/work/boles/final/prep-data-for-bv-calculation.R")

## 3) predict heights
## adds to prepped
source("~/work/boles/final/htsFINAL.R")

## 4) predict bole volumes
## Creates: "~/work/data/data/boles/boles.csv"
source("~/work/boles/final/bolevolumefinal.R")

## 5) Check values
source("~/work/boles/final/checkData.R")

