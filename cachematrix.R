## The function creates a 'matrix' object that can cache its inverse.
## Rather than computing the inverse repeatly, we can try caching its inverse by 
## using lexical scopings. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Before calculating the inverse matrix, the function can check if the inverse has
## already been calculated. If so, it can retrieve the calculated answer ranther than
## calculate it again. This process can save running time.

cacheSolve <- function(x, ...) {
## return an inverse matrix
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    res <- x$get()
    i <- solve(res)
    x$setinverse(i)
    i
}
