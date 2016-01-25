## Matrix Inversion is usually a costly Computation and hence there may be 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## Below is a pair of functions cache the inverse of a matrix

# This function creates a special maxtrix object that can cache its inverse.
# It is a list containing a fucntion to 
# set the matrix
# get the matrix
# set the Inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y){
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse_matrix <<-solve
        getinverse <- function() inverse_matrix
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
        
}


# This function calculates the inverse of the special matrix created by 
# makeCacheMatrix. It will first checks to see if the inverse has already been
# calculated. If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets its inverse in the
# cache via the setinverse function

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached inverse")
                return(inverse_matrix)
        }
        
        datamatrix <- x$get()
        inverse_matrix <- solve(datamatrix, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}