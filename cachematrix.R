## The first function - makeCacheMatrix creates a matrix which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
## The second function - cacheSolve returns the inverse of the matrix. However, it first checks to see if the inverse has already been calculated and stored in cache. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the 
##inverse of the matrix and sets the inverse matrix in the cache via the setinv function.

## To run the R script functions - 
##a. Create and assign a "non singular, invertible square matrix" to a variable (say 'mat')
##b. Call the makeCacheMatrix to compute the inverse of 'mat' and assign result to a different variable (say 'ret')
##c. Do repeated calls to cacheSolve(ret) to see the cached values

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve  
##retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) # Return inverse matrix that has been cached
    }
   
    data <- x$get()
    #Compute the inverse of matrix in 'data'
    m <- solve(data, ...)  
    x$setinv(m)
    m #Return a matrix that is the inverse of 'x'
}
