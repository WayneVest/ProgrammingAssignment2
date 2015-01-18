## The functions makeCacheMatrix and cacheSolve work together to store matrix values, including caching the inverse of the stored matrix after
## the first calculation of said inverse.

## The makeCacheMatrix function emulates a class by providing methods to set and get a base value, as well as the inverse value of the base.
## The included methods are:
    ## get() - to get the current matrix
    ## set() - to set a new matrix
    ## setInverse() - stores a matrix, assumed to be the inverse of the matrix returned at get()
    ## getInverse() - to retrieve the cached matrix stored at setInverse()
## NOTE: there are no metods to acutally calculate the inverse - it is assumed this is done correctly and passed-in correctly

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(inverse) {
        print("here")
        i <<- inverse
        print(i)
        print("done")
    }
    getInverse <- function(){
        i
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Gets the inverse of a given makeCacheMatrix matrix, either either by calculation from cache if it has previously been calculated.
## NOTE: a makeCacheMatrix matrix is required as cacheSolve expects the passed-in data structre to have makeCacheMatrix methods

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
