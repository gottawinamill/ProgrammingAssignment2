## JV 2014-05-19 cachematrix.R
## this file consists of 2 functions 
## makeCacheMatrix - this creates a list object from a matrix
	## which caches 1: the matrix, 2: the inverse of the matrix and 3:the digest of the matrix whixh is used to check that the matrix has not changed
## cacheSolve - this is a simple function which checks that a valid inverse is available in cache
	## if so this is returned, otherwise it calls makeCacheMatrix to calculate the inverse, caches the value and returns  	

## function to create cachable matrix object
makeCacheMatrix <- function(x = matrix()) {
	dig <- NULL ## variable which holds digest of matrix
	inv <- NULL ## variable which holds inverse of matrix


	## getter and setters for matrix object
	 set <- function(y) {
                x <<- y
                inv <<- NULL
		dig <<- NULL
        }
        get <- function() x 
	
	## getters and setters for digest
	setDigest <- function(y){
		library(digest)
		dig <<- digest(y)
	}

	getDigest <- function()dig 

	## getters and setters for matrix inverse

	setInv <- function(y){
		inv <<- y
	}

	getInv <- function()inv

	list(set = set, get = get,
		setInv = setInv, getInv = getInv,
		setDigest = setDigest, getDigest = getDigest)
	
}

##function to calcualte matrix inverse and store / retrieve from cache
cacheSolve <- function(x,...){
	
	## if any of the 3 following conditions are met then we need to recalculate both digest and matrix
	## and cache and return the inverse
	## conditions 1: digest isn't cached, 2: inverse isn't cached, 3: cached digest doesn't match new value

	##use of plain identical(a,b) can lead to false results as by default it ignores attributes - see http://stackoverflow.com/questions/7585316/identical-data-frames-with-different-digests-in-r
	## so we need to calculate and cache the digest also (this could be a long running operation)
	## we can then use is.identical(digest(a),digest(b)) to determine if we are really dealing with the same matrix (for simplicity sake I'm ignoring hash collisions)

	library(digest)
	if(is.null(x$getDigest()) || is.null(x$getInv())  || 
		!identical(x$getDigest(), digest(x$get()))){
				
		x$setInv(solve(x$get()))
		x$setDigest(x$get())
		
		return(x$getInv())
	}
	x$getInv()
	  
}
