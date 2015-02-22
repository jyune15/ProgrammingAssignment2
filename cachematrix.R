## makeCacheMatrix is a fuction for setting of matrix before invert, 
## input of makeCacheMatrix fuction is a matrix X
## then, the result of makeCacheMatrix(X) becomes a input for cacheSolve()
## cacheSolve() function make a inverted matrix of X.
## if the inverted matrix is calculated previously, 
## it doesn't calculated again but return previously calculated result on cache memory

## makeCacheMatrix is a function for setting of matrix before invert fuction ("casheSolve")

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a fuction for invert of matrix X

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) 
        x$setinverse(m)
        m     
}
