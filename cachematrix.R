## makeCacheMatrix creates a special matrix object that can cache its inverse
## this function is designed to work with invertible square matrices.
makeCacheMatrix <- function(x = matrix()) {
        ## check whether the matrix is square, otherwise print an error and quit.
        if(dim(x)[1] != dim(x)[2]) {
            print ("Only square matrices are valid for this function")
        }
        ## for square matrices, create the object that can cache the inverse.
        else {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }   
            get <- function() x
            setinverse <- function(solved) inv <<- solved
            getinverse <- function() inv
            list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
        }
}


## cacheSolve returns the inverse of a matrix from cache if it has previously been cached,
## otherwise calculates and returns the inverse.
cacheSolve <- function(x, ...) {       
        ## get the cached inverse
        inv <- x$getinverse()        
        ## if cached is not null, use and display the cached value prompting it is cached
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If not cached, compute and cache the inverse for later use
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)       
        ## Display the calculated inverse
        inv
}
