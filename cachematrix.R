## Put comments here that give an overall description of what your
## functions do

## First function gets "makeCacheMatrix", creates a matrix x passed as an arguement to it
## inv is the cached inverse matrix which is set outside the environment of the function call of cacheSolve

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
           x <<- y
           inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## THe second function when called checks first if a cached inverse exists and if so prints it. Else it creates the inverse and stores in the globa environment as cached inv

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
