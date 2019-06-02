## Function makeCacheMatrix creates a special "matrix", which is a list containing a function to 
## set and get the value of the matrix and
## set and get the value of the inverse  

makeCacheMatrix <- function(mat = matrix()) {
        m <- NULL
        set <- function(y) {
                mat <<- y
                m <<- NULL
        }
        get <- function() mat
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve calculates the inverse of the special "matrix". 
## Checks to see if the inverse has already been calculated. 
##   If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the original matrix and sets that value in the cache using setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {x
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
