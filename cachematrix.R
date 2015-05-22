## The 2 functions here calculate the inverse of a square INVERTIBLE matrix 
## retrieving the inverse  from a stored cache if already calculated. 


## makeCacheMatrix builds a cache environment, and 2 matrices 
## in  cache env for storing the input matrix and the inverse matrix.
## also makeCacheMatrix returns  in  global env a list containing 
## 4 function bound to cache env used by cacheSolve to get or set 
## the input matrix and the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {

	## store the matrix in  working env and inizialize 
	## the cached inverse to NULL
	
	inverse <- NULL
	set <- function(y) {
			x <<- y
			inverse <<- NULL
		            }

	##  get the value of the matrix
	get <- function() x

	## invert the matrix and store in the cache 
	setsolve <- function(solve) inverse <<- solve

	## get the inverted matrix from the cache
	getsolve <- function() inverse
        
	## return the created functionto the working env 
	## with names for referral via $ 
	list(set=set, get = get,
	setsolve = setsolve,
	getsolve = getsolve)
}


## cacheSolve calculates the inverse of the matrix stored in cache 
## and stores it in cache, if inverted matrix does not exist in cache
## inverted matrix is create in working env and its value is stored in cache
## cacheSolve retrieves the inverted matrix from cache,
## if inverted matrix was already stored in it

cacheSolve <- function(x, ...) {

		## retrieve the cached value of the inverted matrix        
		inverse <- x$getsolve()
		if(!is.null(inverse)) {
				## if  a value is retrieved, 
				## print the value at console and 								## the info that it comes from cache 

				message("getting cached data")
				return(inverse)
				}
		## if no value in cache, assign cached matrix value 
		## to local variable data        
		data <- x$get()
		
		## calculate the inverted value of the matrix
		inverse <- solve(data, ...)
		
		## store the inverted value in cache
		x$setsolve(inverse)

		## display the inverted matrix on console
		inverse
}