## Put comments here that give an overall description of what your
## functions do

## make a cache matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
                
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## return a matrix that is the inverse of 'x
cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

c=rbind(c(1, -1/4), c(-1/4, 1))  
m <- makeCacheMatrix(c)
z<-cacheSolve(m)
z
