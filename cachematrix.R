## This code caches the inverse of a matrix.

## This first function creates the matrix object.

makeCacheMatrix <- function(x = matrix()) {

                m <- NULL
                set <- function(y) 
                {
                        x <<- y
                        m <<- NULL
                }

                get <- function() x
                setInv <- function(i) m <<- solve(x)
                getInv <- function() m
                list(set = set, get = get, 
                        setInv = setInv, 
                        getInv = getInv)
}


## This second function caches the inverse of the matrix created in the function above and returns the inverted matrix.

cacheSolve <- function(x, ...) {

        m <- x$getInv()
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get())
        x$setInv(m)
        m
}
