### table of coefficients for dbh to ht calculations

species.table <- list("fagr","prpe","acsp","acsa","acpe","abba","abba","abba","abba","abba","abba",
	"soam","soam","soam","piru","piru","piru","piru","piru","piru","beal","beal","beal",
	"beco","beco","beco","beco","beco","beco")

elev.table <- list("all","all","all","all","all","low","low","mid","mid","high","high",
	"low","mid","high","low","low","mid","mid","high","high","low","low","mid","low","low",
	"mid","mid","high","high")

canopy.table <- list("all","all","all","all","all","below","above","below","above","below","above",
	"all","all","all","below","above","below","above","below","above","below","above","all",
	"below","above","below","above","below","above")

a.table <- c(coef(fagrlow.mod)[[1]],coef(prpelow.mod)[[1]],coef(acsplow.mod)[[1]],coef(acsalow.mod)[[1]],
	coef(acpelow.mod)[[1]],coef(abbalowbelow.mod)[[1]],coef(abbalowabove.mod)[[1]],coef(abbamidbelow.mod)[[1]],
	coef(abbamidabove.mod)[[1]],coef(abbahighbelow.mod)[[1]],coef(abbahighabove.mod)[[1]],coef(soamlow.mod)[[1]],
	coef(soammid.mod)[[1]],coef(soamhigh.mod)[[1]],coef(pirulowbelow.mod)[[1]],coef(pirulowabove.mod)[[1]],
	coef(pirumidbelow.mod)[[1]],coef(pirumidabove.mod)[[1]],coef(piruhighbelow.mod)[[1]],coef(piruhighabove.mod)[[1]],
	coef(beallowbelow.mod)[[1]],coef(beallowabove.mod)[[1]],coef(bealmid.mod)[[1]],
	coef(becolowbelow.mod)[[1]],coef(becolowabove.mod)[[1]],coef(becomidbelow.mod)[[1]],coef(becomidabove.mod)[[1]],
	coef(becohighbelow.mod)[[1]],coef(becohighabove.mod)[[1]])
	
b.table <- c(coef(fagrlow.mod)[[2]],coef(prpelow.mod)[[2]],coef(acsplow.mod)[[2]],coef(acsalow.mod)[[2]],
	coef(acpelow.mod)[[2]],coef(abbalowbelow.mod)[[2]],coef(abbalowabove.mod)[[2]],coef(abbamidbelow.mod)[[2]],
	coef(abbamidabove.mod)[[2]],coef(abbahighbelow.mod)[[2]],coef(abbahighabove.mod)[[2]],coef(soamlow.mod)[[2]],
	coef(soammid.mod)[[2]],coef(soamhigh.mod)[[2]],coef(pirulowbelow.mod)[[2]],coef(pirulowabove.mod)[[2]],
	coef(pirumidbelow.mod)[[2]],coef(pirumidabove.mod)[[2]],coef(piruhighbelow.mod)[[2]],coef(piruhighabove.mod)[[2]],
	coef(beallowbelow.mod)[[2]],coef(beallowabove.mod)[[2]],coef(bealmid.mod)[[2]],
	coef(becolowbelow.mod)[[2]],coef(becolowabove.mod)[[2]],coef(becomidbelow.mod)[[2]],coef(becomidabove.mod)[[2]],
	coef(becohighbelow.mod)[[2]],coef(becohighabove.mod)[[2]])

dbhtoht.table <- cbind(species.table,elev.table,canopy.table,a.table,b.table)
write.csv(dbhtoht.table,"coefsDBHtoHT.csv")






