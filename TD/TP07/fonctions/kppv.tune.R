kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{
	taux <- rep(0,length(nppv))

	for (k in nppv)
	{
	  pred <- kppv.val(Xapp, as.factor(zapp), k, Xval)
	  errRate <- 0
	  for(i in length(zval)){
	    if(pred[i]!=zval[i]){
	      errRate = errRate + 1
	    }
	  }
    taux[k] <- errRate/length(zval)
	}

  Kopt <- apply(taux, which.min)
  Kopt
}
