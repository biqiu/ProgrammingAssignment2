makeCacheMatri <- function(old_matrix = matrix()) {
	inverse_matrix <- NULL
	set <- function(ma) {
		old_matrix <<- ma
		inverse_matrix <<- NULL
	}
	get <- function() old_matrix
	setInverse <- function(inverse) inverse_matrix <<- inverse
	getInverse <- function() inverse_matrix
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 

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