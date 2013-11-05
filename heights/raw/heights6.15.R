### 6/15 adding heights for species

pp <- read.csv("C:/Documents and Settings/Noah/My Documents/Dropbox/Noah/ppnew.csv",header=T,sep=",")

nrow(pp[pp$SPECIES=="PIRU"&!is.na(pp$DBH10)&is.na(pp$HT10),])

a <- levels(pp$SPECIES)
b <- vector()
for(i in 1:length(a)) {
	b<- append(b,nrow(pp[pp$SPECIES==a[[i]],]))
}
paste(a,b)

### possible species 
### BECO sample size of 945
### ACPE sample size of 190
### SOAM sample size of 252
### ACSP sample size of 105
### BEAL sample size of 116
