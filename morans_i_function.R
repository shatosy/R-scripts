# Creates mean Bray-Curtis similarity based on rarefied "Sample by species" table
# Arguments: "data" is a data matrix with samples as rows and OTU, genes, or species as columns, 
# "perms" is the number of permutations you want to run (Defaults to 999)

print("Loading required package: vegan")
library(vegan)
multi.rarefy<-function(data,perms=999){
	# Initialize Bray-Curtis variable
	bc.sum=0
	for(i in 1:perms)
	{
		# Rarefy data table
		r.data=rrarefy(data,min(rowSums(data)))
		# Generate Bray-Curtis matrix for rarefied table
		r.data.bc=as.matrix(1-(vegdist(r.data)))
		# Sum new Bray-Curtis matrix to previous Bray-Curtis matrix
		bc.sum=bc.sum+r.data.bc
	}

	#Average of the Bray-Curtis similarities
	bc.mean=bc.sum/i
	return(bc.mean)
}
