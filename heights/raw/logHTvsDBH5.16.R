### 5/16 
### log transformed ht vs dbh 1998 fits
### log(ht) = log(a) + b*log(dbh)


### log transformed data, trees with 0 dbh notincluded
htn0 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98!=0&!is.na(abba$HT98),]$HT98
dbhn0 <- abba[abba$ELEV=="L"&!is.na(abba$DBH98)&abba$DBH98!=0&!is.na(abba$HT98),]$DBH98
loght <- log(htn0)
logdbh <- log(dbhn0)
thing <- cbind(logdbh,loght)
thing <- as.data.frame(thing)

### root mean square error to compare models
rmse <- function(obs, pred) sqrt(mean((obs-pred)^2))
windows()
plot(logdbh,loght,main = "Log transformed HT and DBH, linear fit 
for values where logDBH > 1")
log.fit <- lm(thing[logdbh>1,]$loght~logdbh[logdbh>1])
abline(log.fit)


thing2 <- thing[thing$logdbh>1,]
windows()
plot(thing2$logdbh,thing2$loght,main = "Log transformed HT and DBH, linear fit 
for values where logDBH > 1")
log1.fit <- lm(thing2$loght~thing2$logdbh)
abline(log1.fit)
log1.fit.rmse <- rmse(thing2$loght,fitted.values(log1.fit))
legend(x=1.1, y=2.5,legend =paste("RMSE=",log1.fit.rmse,sep=","))




