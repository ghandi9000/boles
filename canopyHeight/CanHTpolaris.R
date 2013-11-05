### 5/17 estimate canopy heights for all permanent plots
### Use the mean height of codominant trees in each plot
### to predict the canopy height for that plot and add a  new
### column containing canopy height
### input.directory <- "C:\\Users\\David\\Dropbox\\Shared\\Datasets"
### work.directory <- "C:\\Users\\David\\Dropbox\\Shared\\BVfix"
begin.time <- Sys.time()
### setwd(input.directory)
pp <- read.csv("pptr.csv")

### NOTE :: Run the name fixing script to get the old names from previous dataset versions ####
######### fix column names

names(pp)[which(names(pp)=="SPEC")] <- "SPECIES"
names(pp)[which(names(pp)=="PPLOT")] <- "PLOT"
names(pp)[which(names(pp)=="SPLOT")] <- "SUBPLOT"
names(pp)[which(names(pp)=="CPOS87")] <- "CRPOS87"
names(pp)[which(names(pp)=="CPOS88")] <- "CRPOS88"
names(pp)[which(names(pp)=="CPOS98")] <- "CRPOS98"
names(pp)[which(names(pp)=="CPOS10")] <- "CRPOS10"

names(pp)[72]<- "ELEVATIO"
names(pp)[73]<- "ELEV"

pp <- subset(pp, !is.na(PLOT))

### setwd(work.directory)

### mean heights by crpos for 2010 trees
o.ht <- mean(pp[pp$CRPOS10 == "o",]$HT10,na.rm=T)
i.ht <- mean(pp[pp$CRPOS10 == "i",]$HT10,na.rm=T)
c.ht <- mean(pp[pp$CRPOS10 == "c",]$HT10,na.rm=T)
d.ht <- mean(pp[pp$CRPOS10 == "d",]$HT10,na.rm=T)
avghts <- c(o.ht,i.ht,c.ht,d.ht)

plots <- c(4:27) ### plots to do
pp$CANHT10 <- rep(NA,nrow(pp)) ### new column for canopy height 2010
### Loop should return plot, sample size (# of "c" trees), and canopy height
canhtsc <- vector()
canhtsd <- vector()
canhtsi <- vector()
sampsizesc <- vector()
sampsizesd <- vector()
sampsizesi <- vector()
plotnums <- vector()

for(i in 1:length(plots)) {
	canhtc <- 0
	sampsizec <- 0
	canhtd <- 0
	sampsized <- 0
	canhti <- 0
	sampsizei <- 0
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT10[[j]]) & pp$CRPOS10[[j]]=="c") {
				canhtc <- canhtc + pp$HT10[[j]];
				sampsizec <- sampsizec + 1;
			}
		}
	}
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT10[[j]]) & pp$CRPOS10[[j]]=="d") {
				canhtd <- canhtd + pp$HT10[[j]];
				sampsized <- sampsized + 1;
			}
		}
	}
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT10[[j]]) & pp$CRPOS10[[j]]=="i") {
				canhti <- canhti + pp$HT10[[j]];
				sampsizei <- sampsizei + 1;
			}
		}
	}
	canhtc <- canhtc/sampsizec
	canhtd <- canhtd/sampsized
	canhti <- canhti/sampsizei
	canhtsi <- append(canhtsi,canhti)
	canhtsc <- append(canhtsc,canhtc)
	canhtsd <- append(canhtsd,canhtd)
	sampsizesi <- append(sampsizesi,sampsizei)
	sampsizesc <- append(sampsizesc,sampsizec)
	sampsizesd <- append(sampsizesd,sampsized)
	plotnums <- append(plotnums,plots[[i]])
}

canopy <- data.frame(plotnums,sampsizesi,canhtsi,sampsizesc,canhtsc,sampsizesd,canhtsd)
names(canopy) <- c("plot","#Int","HtInt","#Codom","HtCodom","#Dom","HtDom")
#### Note: in Plot 9 intermediate trees mean height greater than codominant tree height

### add data to the CANHT10 column in pp
for(i in 1:nrow(canopy)) {
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == canopy$plot[[i]]) { pp$CANHT10[[j]] <- canopy$HtCodom[[i]] }
	}
}
################### Make CANHT98 column #################################
plots <- c(4:27) ### plots to do
pp$CANHT98 <- rep(NA,nrow(pp)) ### new column for canopy height 1998
### Loop should return plot, sample size (# of "c" trees), and canopy height
canhtsc <- vector()
canhtsd <- vector()
canhtsi <- vector()
sampsizesc <- vector()
sampsizesd <- vector()
sampsizesi <- vector()
plotnums <- vector()

for(i in 1:length(plots)) {
	canhtc <- 0
	sampsizec <- 0
	canhtd <- 0
	sampsized <- 0
	canhti <- 0
	sampsizei <- 0
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT98[[j]]) & pp$CRPOS98[[j]]=="c") {
				canhtc <- canhtc + pp$HT98[[j]];
				sampsizec <- sampsizec + 1;
			}
		}
	}
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT98[[j]]) & pp$CRPOS98[[j]]=="d") {
				canhtd <- canhtd + pp$HT98[[j]];
				sampsized <- sampsized + 1;
			}
		}
	}
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT98[[j]]) & pp$CRPOS98[[j]]=="i") {
				canhti <- canhti + pp$HT98[[j]];
				sampsizei <- sampsizei + 1;
			}
		}
	}
	canhtc <- canhtc/sampsizec
	canhtd <- canhtd/sampsized
	canhti <- canhti/sampsizei
	canhtsi <- append(canhtsi,canhti)
	canhtsc <- append(canhtsc,canhtc)
	canhtsd <- append(canhtsd,canhtd)
	sampsizesi <- append(sampsizesi,sampsizei)
	sampsizesc <- append(sampsizesc,sampsizec)
	sampsizesd <- append(sampsizesd,sampsized)
	plotnums <- append(plotnums,plots[[i]])
}

canopy <- data.frame(plotnums,sampsizesi,canhtsi,sampsizesc,canhtsc,sampsizesd,canhtsd)
names(canopy) <- c("plot","#Int","HtInt","#Codom","HtCodom","#Dom","HtDom")

### add data to the CANHT98 column in pp
for(i in 1:nrow(canopy)) {
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == canopy$plot[[i]]) { pp$CANHT98[[j]] <- canopy$HtCodom[[i]] }
	}
}
################### Make CANHT86 column #################################
plots <- c(4:27) ### plots to do
pp$CANHT86 <- rep(NA,nrow(pp)) ### new column for canopy height 1986
### Loop should return plot, sample size (# of "c" trees), and canopy height
canhtsc <- vector()
canhtsd <- vector()
canhtsi <- vector()
sampsizesc <- vector()
sampsizesd <- vector()
sampsizesi <- vector()
plotnums <- vector()

for(i in 1:length(plots)) {
	canhtc <- 0
	sampsizec <- 0
	canhtd <- 0
	sampsized <- 0
	canhti <- 0
	sampsizei <- 0
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT86[[j]]) & pp$CRPOS88[[j]]=="c") {
				canhtc <- canhtc + pp$HT86[[j]];
				sampsizec <- sampsizec + 1;
			}
		}
	}
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT86[[j]]) & pp$CRPOS88[[j]]=="d") {
				canhtd <- canhtd + pp$HT86[[j]];
				sampsized <- sampsized + 1;
			}
		}
	}
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT86[[j]]) & pp$CRPOS88[[j]]=="i") {
				canhti <- canhti + pp$HT86[[j]];
				sampsizei <- sampsizei + 1;
			}
		}
	}
	canhtc <- canhtc/sampsizec
	canhtd <- canhtd/sampsized
	canhti <- canhti/sampsizei
	canhtsi <- append(canhtsi,canhti)
	canhtsc <- append(canhtsc,canhtc)
	canhtsd <- append(canhtsd,canhtd)
	sampsizesi <- append(sampsizesi,sampsizei)
	sampsizesc <- append(sampsizesc,sampsizec)
	sampsizesd <- append(sampsizesd,sampsized)
	plotnums <- append(plotnums,plots[[i]])
}

canopy <- data.frame(plotnums,sampsizesi,canhtsi,sampsizesc,canhtsc,sampsizesd,canhtsd)
names(canopy) <- c("plot","#Int","HtInt","#Codom","HtCodom","#Dom","HtDom")

### add data to the CANHT86 column in pp
for(i in 1:nrow(canopy)) {
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == canopy$plot[[i]]) { pp$CANHT86[[j]] <- canopy$HtCodom[[i]] }
	}
}

################### Make CANHT87 column #################################
plots <- c(4:27) ### plots to do
pp$CANHT87 <- rep(NA,nrow(pp)) ### new column for canopy height 1987
### Loop should return plot, sample size (# of "c" trees), and canopy height
canhtsc <- vector()
canhtsd <- vector()
canhtsi <- vector()
sampsizesc <- vector()
sampsizesd <- vector()
sampsizesi <- vector()
plotnums <- vector()

for(i in 1:length(plots)) {
	canhtc <- 0
	sampsizec <- 0
	canhtd <- 0
	sampsized <- 0
	canhti <- 0
	sampsizei <- 0
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT87[[j]]) & pp$CRPOS87[[j]]=="c") {
				canhtc <- canhtc + pp$HT87[[j]];
				sampsizec <- sampsizec + 1;
			}
		}
	}
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT87[[j]]) & pp$CRPOS87[[j]]=="d") {
				canhtd <- canhtd + pp$HT87[[j]];
				sampsized <- sampsized + 1;
			}
		}
	}
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == plots[[i]]) {
			if(!is.na(pp$HT87[[j]]) & pp$CRPOS87[[j]]=="i") {
				canhti <- canhti + pp$HT87[[j]];
				sampsizei <- sampsizei + 1;
			}
		}
	}
	canhtc <- canhtc/sampsizec
	canhtd <- canhtd/sampsized
	canhti <- canhti/sampsizei
	canhtsi <- append(canhtsi,canhti)
	canhtsc <- append(canhtsc,canhtc)
	canhtsd <- append(canhtsd,canhtd)
	sampsizesi <- append(sampsizesi,sampsizei)
	sampsizesc <- append(sampsizesc,sampsizec)
	sampsizesd <- append(sampsizesd,sampsized)
	plotnums <- append(plotnums,plots[[i]])
}

canopy <- data.frame(plotnums,sampsizesi,canhtsi,sampsizesc,canhtsc,sampsizesd,canhtsd)
names(canopy) <- c("plot","#Int","HtInt","#Codom","HtCodom","#Dom","HtDom")

### add data to the CRPOS87 column in pp
for(i in 1:nrow(canopy)) {
	for(j in 1:nrow(pp)) {
		if(pp$PLOT[[j]] == canopy$plot[[i]]) { pp$CANHT87[[j]] <- canopy$HtCodom[[i]] }
	}
}

###########################################################
#####		Canopy height for plot 16 in 1987		#####
##### 	No codom trees were measured for 		#####
##### 	height, so using 1998 data instead		#####
###########################################################
for(i in 1:nrow(pp)) {
	if(pp$PLOT[[i]] == 16)  {
		pp$CANHT87[[i]] <- pp$CANHT98[[i]]
	}
}

###setwd(work.directory)
write.csv(pp,"canopy.csv")

end.time <- Sys.time()

runtime <- end.time - begin.time

runtime




