##############################################################
########								########	
########	Fix Bole Volume Calculations so trees	########	
########	that had a height in previous year have	########	
########	growth added on to the previous height:	########	
########	requires new fits of growth 			########	
########			9/10/2012				########	
##############################################################

input.directory <- "C:\\Users\\David\\Dropbox\\Shared\\BVfix"
work.directory <- "C:\\Users\\David\\Dropbox\\Shared\\BVfix"

setwd(input.directory)
pp <- read.csv("canopy.csv")


