##Put comments here that give an overall decscription of what your
##functions do
##The purpose these two functions is getting inverse matrix quickly and directly,
##when there is an inverse matrix in the cache and the original matrix was not be
##changed.

##Write a short comment describing this function
##The purpose of this function is as follows:
##1.set matrix
##2.get matrix
##3.set inverse matrix
##4.get inverse matrix
makeCacheMatri <- function(old_matrix = matrix()) {
	inverse_matrix <- NULL
	set <- function(ma) {
		old_matrix <<- ma
		inverse_matrix <<- NULL
	}
	get <- function() old_matrix
	setInverse <- function(inverse) inverse_matrix <<- inverse
	getInverse <- function() inverse_matrix
	list(set = set, get = get, 
	     setInverse = setInverse, 
	     getInverse = getInverse)
} 

##Write a short comment describing this function
##The purpose of this function is as follows:
##1.If the original matrix was not be changed and there is an inverse matrix in
##the cache, it will return the inverse matrix directly.
##2.If there is no inverse matrix in the cache, It will use solve() function to
##get inverse matrix and then write the inverse matrix in the cache for next using.

cacheSolve <- function(old_matrix, ...) {
	inverse_matrix <- old_matrix$getInverse()
	if(!is.null(inverse_matrix)) {
		message("getting cache data")
		return(inverse_matrix)
	}
	data <- old_matrix$get()
	inverse_matrix <- solve(data)
	old_matrix$setInverse(inverse_matrix)
	inverse_matrix
}