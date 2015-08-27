# Reads through the columns of a matrix with replicate samples as rows and returns a matrix with the minimum value for each sample
# Matrix is the data matrix and index is the column containing the sample indices

min.matrix.row <- function(matrix, index){
	min.matrix = matrix(NA, length(unique(index)), ncol(matrix))
	for(i in 1:ncol(matrix)){
		min.matrix[,i] = tapply(matrix[,i], index, min)
		}
	return(min.matrix)
}

# Reads through the columns of a matrix with replicate samples as rows and returns a matrix with the maximum value for each sample
# Matrix is the data matrix and index is the column containing the sample indices

max.matrix.row <- function(matrix, index){
	max.matrix = matrix(NA, length(unique(index)), ncol(matrix))
	for(i in 1:ncol(matrix)){
		max.matrix[,i] = tapply(matrix[,i], index, max)
		}
	return(max.matrix)
}
