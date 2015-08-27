# Reads through the columns of a matrix with replicate samples as rows and adds the replicate samples together. 
# 'matrix' is the data matrix with samples as rows. 
# 'index' is the sample id's. Returns a matrix with samples as rows and the values are the summed replicates.

sum.matrix.rows<-function(matrix, index){
	summed.matrix = matrix(NA, length(unique(index)), ncol(matrix))
	for(i in 1:ncol(matrix)){
		summed.matrix[,i] = tapply(matrix[,i], index, sum)
		}
	return(summed.matrix)
}

# Reads through the columns of a matrix with replicate samples as rows and calculates the mean of the replicate samples together.
# 'matrix' is the data matrix with samples as rows. 
# 'index' is the sample id's. Returns a matrix with samples as rows and the values are the averaged replicates.

mean.matrix.rows<-function(matrix, index){
	mean.matrix = matrix(NA, length(unique(index)), ncol(matrix))
	for(i in 1:ncol(matrix)){
		mean.matrix[,i] = tapply(matrix[,i], index, mean)
		}
	return(mean.matrix)
}
