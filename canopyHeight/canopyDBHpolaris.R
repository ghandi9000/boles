
begin.time <- Sys.time()

### Polaris version
pp <- read.csv("canopy.csv",header=T,sep=",")

### loop to get mean dbh for trees at or above canopy height in each plot
plots <- c(4:27) # plots to do

### loops returns plot number, sample size, and mean dbh of canopy trees
meandbhs <- vector()
sampsizes <- vector()
plotnums <- vector()

for(i in 1:length(plots)) {
	meandbh <- 0
	sampsize <- 0
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]]==plots[[i]]) {
			if(!is.na(pp$HT98[[j]]) &!is.na(pp$DBH98[[j]]) & pp$HT98[[j]] >= pp$CANHT98[[j]]) {
				sampsize <- sampsize + 1
				meandbh <- meandbh + pp$DBH98[[j]]
			}
		}
	}
	meandbh <- meandbh/sampsize
	meandbhs <- append(meandbhs,meandbh)
	sampsizes <- append(sampsizes,sampsize)
	plotnums <- append(plotnums,plots[[i]])
}

candbhs <- data.frame(plotnums,sampsizes,meandbhs)
names(candbhs) <- c("plot","sampsize","candbh")

### add data to the CANDBH98 column in pp
pp$CANDBH98 <- rep(NA,nrow(pp))
for(i in 1:nrow(candbhs)) {
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == candbhs$plot[[i]]) { pp$CANDBH98[[j]] <- candbhs$candbh[[i]] }
	}
}

######################### 2010 ############################################
### loop to get mean dbh for trees at or above canopy height in each plot
plots <- c(4:27) # plots to do

### loops returns plot number, sample size, and mean dbh of canopy trees
meandbhs <- vector()
sampsizes <- vector()
plotnums <- vector()

for(i in 1:length(plots)) {
	meandbh <- 0
	sampsize <- 0
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]]==plots[[i]]) {
			if(!is.na(pp$HT10[[j]]) &!is.na(pp$DBH10[[j]]) & pp$HT10[[j]] >= pp$CANHT10[[j]]) {
				sampsize <- sampsize + 1
				meandbh <- meandbh + pp$DBH10[[j]]
			}
		}
	}
	meandbh <- meandbh/sampsize
	meandbhs <- append(meandbhs,meandbh)
	sampsizes <- append(sampsizes,sampsize)
	plotnums <- append(plotnums,plots[[i]])
}

candbhs <- data.frame(plotnums,sampsizes,meandbhs)
names(candbhs) <- c("plot","sampsize","candbh")

### add data to the CANDBH10 column in pp
pp$CANDBH10 <- rep(NA,nrow(pp))
for(i in 1:nrow(candbhs)) {
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == candbhs$plot[[i]]) { pp$CANDBH10[[j]] <- candbhs$candbh[[i]] }
	}
}

######################### 1986 ############################################
### loop to get mean dbh for trees at or above canopy height in each plot
plots <- c(4:27) # plots to do

### loops returns plot number, sample size, and mean dbh of canopy trees
meandbhs <- vector()
sampsizes <- vector()
plotnums <- vector()

for(i in 1:length(plots)) {
	meandbh <- 0
	sampsize <- 0
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]]==plots[[i]]) {
			if(!is.na(pp$HT86[[j]]) &!is.na(pp$DBH86[[j]]) & pp$HT86[[j]] >= pp$CANHT86[[j]]) {
				sampsize <- sampsize + 1
				meandbh <- meandbh + pp$DBH86[[j]]
			}
		}
	}
	meandbh <- meandbh/sampsize
	meandbhs <- append(meandbhs,meandbh)
	sampsizes <- append(sampsizes,sampsize)
	plotnums <- append(plotnums,plots[[i]])
}

candbhs <- data.frame(plotnums,sampsizes,meandbhs)
names(candbhs) <- c("plot","sampsize","candbh")

### add data to the CANDBH86 column in pp
pp$CANDBH86 <- rep(NA,nrow(pp))
for(i in 1:nrow(candbhs)) {
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == candbhs$plot[[i]]) { pp$CANDBH86[[j]] <- candbhs$candbh[[i]] }
	}
}

######################### 1987 ############################################33333
### loop to get mean dbh for trees at or above canopy height in each plot
plots <- c(4:27) # plots to do

### loops returns plot number, sample size, and mean dbh of canopy trees
meandbhs <- vector()
sampsizes <- vector()
plotnums <- vector()

for(i in 1:length(plots)) {
	meandbh <- 0
	sampsize <- 0
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]]==plots[[i]]) {
			if(!is.na(pp$HT87[[j]]) &!is.na(pp$DBH87[[j]]) & !is.na(pp$CANHT87[[j]]) & pp$HT87[[j]] >= pp$CANHT87[[j]]) {
				sampsize <- sampsize + 1
				meandbh <- meandbh + pp$DBH87[[j]]
			}
		}
	}
	meandbh <- meandbh/sampsize
	meandbhs <- append(meandbhs,meandbh)
	sampsizes <- append(sampsizes,sampsize)
	plotnums <- append(plotnums,plots[[i]])
}

candbhs <- data.frame(plotnums,sampsizes,meandbhs)
names(candbhs) <- c("plot","sampsize","candbh")

### add data to the CANDBH87 column in pp
pp$CANDBH87 <- rep(NA,nrow(pp))
for(i in 1:nrow(candbhs)) {
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == candbhs$plot[[i]]) { pp$CANDBH87[[j]] <- candbhs$candbh[[i]] }
	}
}

write.csv(pp,"canopy2.csv")

end.time <- Sys.time()

runtime <- end.time - begin.time

runtime




