## This function creates a matrix object with a cachable "inverse" property

makeCacheMatrix <- function(x = matrix()) {
    # set the inverse property of the object to 'NULL'
    i <- NULL
    #the object's set function (the matrix property is set to the new matrix and the inverse property is set to NULL)
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #the object's get function. The matrix is returned
    get <- function() x
    #The object's setinverse function sets the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    #The object's getinverse function returns the inverse of the matrix
    getinverse <- function() i
    #allows access to the obect's functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a cacheMatrix object as an argument and checks to see if it's inverse has been calculated.
## If it has been calculated, it returns that inverse, if it hasn't it works out the inverse, sets it in the object and then returns it.
cacheSolve <- function(x, ...) {
    
    ## gets the inverse property of the cacheMatrix
    i <- x$getinverse()
    ##checks to see if the inverse has been cached, returns it if it has.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ##gets the matrix property of the cacheMatrix object
    data <- x$get()
    ##gets the inverse of the matrix
    i <- solve(data, ...)
    ## sets the inverse in the cacheMatrix object
    x$setinverse(i)
    ##returns the inverse
    i
}
