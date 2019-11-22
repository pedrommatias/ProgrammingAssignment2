# The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve().
# The first function in the file, makeCacheMatrix() creates an R object that stores a
# matrix and its inverse. The second function, cacheSolve() requires an argument that
# is returned by makeCacheMatrix() in order to retrieve the inverse from the cached value
# that is stored in the makeCacheMatrix() object's environment.


# makeCacheMatrix() description
# The function makeCacheMatrix() creates a special "matrix", which is a list
# containing 4 functions: set(), setinv(), get() and getinv(). The makeCacheMatrix()
# environment includes also two data objects, x and b, both are matrices.

makeCacheMatrix <- function(x = matrix()){
        b <- NULL
        set <- function(y){
                x <<- y
                b <<- NULL 
        }
        get <- function() x
        setinv <- function(inv) b <<- inv
        getinv <- function() b
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

# cacheSolve() description
# The function cacheSolve() calculates the inverse of the special "matrix" created
# with makeCacheMatrix(). However, it first checks to see if the inverse has already
# been calculated. If so, it gets the inverse from the cache and skips the
# computation. Otherwise, it calculates the inverse of the data and sets the
# inverse in the cache via the the setinv() function.

cacheSolve <- function(x, ...){
        b <- x$getinv()
        if(!is.null(b)) {
                message("getting cached data")
                return(b)
        }
        data <- x$get()
        # idn = identity matrix with dimension nrow(data) x nrow(data)
        idn <- matrix(nrow = nrow(data), ncol = ncol(data))
        for (i in 1:nrow(data)){
                for(j in 1:ncol(data)){
                        if (i == j){
                                idn[i, j] <- 1
                        }else{
                                idn[i, j] <- 0
                        }
                }
        }
        b <- solve(data, idn)
        x$setinv(b)
        b
}
