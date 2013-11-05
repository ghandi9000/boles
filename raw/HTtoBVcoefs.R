### table of coefficients for ht to bv calculations

species.table <- list("abba","piru","acpe","acsp","beal","beco","prpe","soam","acsa")

a.table <- c(coef(abba.mod)[[1]],coef(piru.mod)[[1]],coef(acpe.mod)[[1]],coef(acsp.mod)[[1]],
	coef(beal.mod)[[1]],coef(beco.mod)[[1]],coef(prpe.mod)[[1]],coef(soam.mod)[[1]],
	coef(acsa.mod)[[1]])
	
b.table <- c(coef(abba.mod)[[2]],coef(piru.mod)[[2]],coef(acpe.mod)[[2]],coef(acsp.mod)[[2]],
	coef(beal.mod)[[2]],coef(beco.mod)[[2]],coef(prpe.mod)[[2]],coef(soam.mod)[[2]],
	coef(acsa.mod)[[2]])

httobv.table <- cbind(species.table,a.table,b.table)


write.csv(httobv.table,"coefs_HT.to.BV.csv")






