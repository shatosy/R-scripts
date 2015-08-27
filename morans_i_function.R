library(ape)
library(vegan)

# Computes Moran's I coeffecicents for time distance classes
# Argument x is a vector of dates
# Argument y is a community matrix with samples as rows and taxa as columns
# Window is the size of time distance binning 
# Skip is the time distance to move the window
# Returns a data frame with time distance class, Moran's I coefficient, and p-value. Caution, p-value doesn't mean much unless the number of 
# observations are equal across time distance class, which more than likely they are not. 
# One can then plot column 1 (time distance class) vs. column 2 (Moran's I coefficient)
moran.analysis<-function(x,y,window,skip){
	corr.array=data.frame(dist.class=numeric(),mor.coef=numeric(),mor.p=numeric())
	# Generate Euclidean time distances 
	x=c(dist(x))
	# Generate comminity similarity (1 - Bray Curtis Distance)
	y=c(1-vegdist(y))
	time.dist=as.matrix(dist(x))
	bot=min(time.dist)
	top=max(time.dist)
	bin=1+(top-bot-window)/skip
	for(i in 1:bin)
	{
		time.dist.bin=(time.dist>bot+(i-1)*skip & time.dist<=(bot+(i-1)*skip+window))
		moran.stat=Moran.I(y,time.dist.bin,scaled=T)
		mor.coef=moran.stat$observed
		mor.p=moran.stat$p.value
		dist.class=i
		corr.array[i,]=c(dist.class,mor.coef,mor.p)
	}
	return(corr.array)
}
