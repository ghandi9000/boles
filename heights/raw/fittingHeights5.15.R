### 5/16 fitting heights to dbh for species at different elevations
### height columns:  CRHT86, CRHT87, HT10, HT11, HT86, HT87, HT88, HT98
### standard nonlinear height to dbh formula
height <- function(a,DBH,b) {
	a * DBH * exp(b)
}
### root mean square error to compare models
rmse <- function(obs, pred) sqrt(mean((obs-pred)^2))
### start by fitting abba at 3 elevations compare nonlinear and linear fits
### with and without trees with 0 dbhs
abba<-subset(pp, SPECIES == "ABBA")
windows()
par(mfrow = c(1,3))
plot(abba[abba$ELEV=="L",]$DBH98,abba[abba$ELEV=="L",]$HT98)
low.fit <- lm(abba[abba$ELEV=="L",]$HT98~abba[abba$ELEV=="L",]$DBH98)
abline(low.fit)
plot(abba[abba$ELEV=="M",]$DBH98,abba[abba$ELEV=="M",]$HT98)
med.fit <- lm(abba[abba$ELEV=="M",]$HT98~abba[abba$ELEV=="M",]$DBH98)
abline(med.fit)
plot(abba[abba$ELEV=="H",]$DBH98,abba[abba$ELEV=="H",]$HT98)
high.fit <- lm(abba[abba$ELEV == "H",]$HT98~abba[abba$ELEV=="H",]$DBH98)
abline(high.fit)


### With zeros, low elevation linear fit
ht <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&!is.na(abba$HT98),]$HT98
dbh <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&!is.na(abba$HT98),]$DBH98
plot(dbh,ht)
low.fit <- lm(ht~dbh)
abline(low.fit)
low.fit.rmse <- rmse(ht,fitted.values(low.fit))
### Zeros excluded Low elevation, linear fit
htn0 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98!=0&!is.na(abba$HT98),]$HT98
dbhn0 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98!=0&!is.na(abba$HT98),]$DBH98
plot(dbhn0,htn0)
low2.fit <- lm(htn0~dbhn0)
abline(low2.fit)
low2.fit.rmse <- rmse(htn0,fitted.values(low2.fit))

########################### Nonlinear Fits ##################################################
### With zeros, low elevation nonlinear fit, intercept set
low.mod <- nls(ht~a*dbh^(b)+1.37,start = list(a=1,b=2))
plot(dbh,ht)
lines(sort(dbh),sort(fitted.values(low.mod)),lwd=2)
low.mod.rmse <- rmse(ht,fitted.values(low.mod))
### With zeros, low elevation nonlinear fit, no intercept
low1.mod <- nls(ht~a*dbh^(b),start = list(a=1,b=2))
plot(dbh,ht)
lines(sort(dbh),sort(fitted.values(low1.mod)),lwd=2)
low1.mod.rmse <- rmse(ht,fitted.values(low1.mod))

### No zeros, low elevation nonlinear fit, w/intercept
low2.mod <- nls(htn0~a*dbhn0^b+1.37,start = list(a=1,b=1))
plot(dbhn0,htn0)
lines(sort(dbhn0),sort(fitted.values(low2.mod)),lwd=2)
low2.mod.rmse <- rmse(htn0,fitted.values(low2.mod))
### No zeros, low elevation nonlinear fit, no intercept
low3.mod <- nls(htn0~a*dbhn0^b,start = list(a=1,b=1))
plot(dbhn0,htn0)
lines(sort(dbhn0),sort(fitted.values(low3.mod)),lwd=2)
low3.mod.rmse <- rmse(htn0,fitted.values(low3.mod))
### summary figure with plots, fits, and rmse on the graphs #########################################
### put residual plots under on second row
windows()
par(mfrow = c(2,4))

plot(dbh,ht,main = "Low Elevation ABBA 1998 Linear Fit, Zero DBH Included")
abline(low.fit)
legend(x = 0,y=20,legend = paste("RMSE=",low.fit.rmse,sep=""))

plot(dbhn0,htn0,main = "Low Elevation ABBA 1998 Linear Fit, Zero DBH Excluded")
abline(low2.fit)
legend(x = 0,y=20,legend = paste("RMSE=",low2.fit.rmse,sep=""))

plot(dbh,ht,main = "Low Elevation ABBA 1998 Nonlinear Fit, Zero DBH Included, 
Intercept set at 1.37")
lines(sort(dbh),sort(fitted.values(low.mod)),lwd=2)
legend(x = 0,y=20,legend = paste("RMSE=",low.mod.rmse,sep=""))

plot(dbh,ht,main = "Low Elevation ABBA 1998 Nonlinear Fit, Zero DBH Included,
No Intercept")
lines(sort(dbh),sort(fitted.values(low1.mod)),lwd=2)
legend(x = 0,y=20, legend = paste("RMSE=",low1.mod.rmse,sep=""))

plot(dbhn0,htn0,main = "Low Elevation ABBA 1998 Nonlinear Fit, Zero DBH Excluded,
Intercept at 1.37")
lines(sort(dbhn0),sort(fitted.values(low2.mod)),lwd=2)
legend(x = 0,y=20, legend = paste("RMSE=",low2.mod.rmse,sep=""))

plot(dbhn0,htn0,main = "Low Elevation ABBA 1998 Nonlinear Fit, Zero DBH Excluded,
No Intercept")
lines(sort(dbhn0),sort(fitted.values(low3.mod)),lwd=2)
legend(x = 0,y=20, legend = paste("RMSE=",low3.mod.rmse,sep=""))

### residuals

### Try nonlinear fits for trees greater than 5 DBH and linear for less than 5 ##########################################3
ht2 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98 >= 2.7&!is.na(abba$HT98),]$HT98
dbh2 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98 >= 2.7&!is.na(abba$HT98),]$DBH98
ht3 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98 < 2.7&!is.na(abba$HT98),]$HT98
dbh3 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98 < 2.7&!is.na(abba$HT98),]$DBH98
ht4 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98!=0&abba$DBH98 < 5&!is.na(abba$HT98),]$HT98
dbh4 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98!=0&abba$DBH98 < 5&!is.na(abba$HT98),]$DBH98


### low elevation nonlinear fit, no intercept
low4.mod <- nls(ht2~a*dbh2^(b),start = list(a=1.4,b=0.7))
plot(dbh2,ht2)
lines(sort(dbh2),sort(fitted.values(low4.mod)),lwd=2)
low4.mod.rmse <- rmse(ht2,fitted.values(low4.mod))

low5.fit <- lm(ht3~dbh3)
low5.fit.rmse <- rmse(ht3,fitted.values(low5.fit))

low6.fit <- lm(ht2~dbh2)

low7.fit <- lm(ht4~dbh4)
rmse(ht4,fitted.values(low7.fit))

low8.mod <- nls(ht3~a*dbh3^(b),start = list(a=1.4,b=0.7))
low8.mod.rmse <- rmse(ht3,fitted.values(low8.mod))
plot(dbh3,ht3,main = "Low Elevation ABBA 1998 Nonlinear Fit for DBH Less than 5,
No Intercept")
lines(sort(dbh3),sort(fitted.values(low8.mod)),lwd=2)
legend(x = 0,y=20, legend = paste("RMSE=",low8.mod.rmse,sep=""))


#### Best fit so far is using linear fit with 0 dbhs included, no intercept, for trees <2.7 DBH
#### and nonlinear fit for trees greater/equal to 2.7 cm dbh
par(mfrow = c(1,2))
plot(dbh2,ht2,main = "Low Elevation ABBA 1998 Nonlinear Fit for DBH Greater/Equal to 2.7,
No Intercept")
lines(sort(dbh2),sort(fitted.values(low4.mod)),lwd=2)
legend(x = 0,y=20, legend = paste("RMSE=",low4.mod.rmse,sep=""))

plot(dbh3,ht3,main = "Low Elevation ABBA 1998 Linear Fit for DBH Less than 5,
No Intercept")
abline(low5.fit)
legend(x = 0,y=2.5, legend = paste("RMSE=",low5.fit.rmse,sep=""))











