## makeCacheMatrix function is really a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                # use '<<-' to assign a value to an object in an environment 
                # that is different from the current environment
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_var) inverse <<- inverse_var
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        inverse = x$getinverse()

        # if the inverse has already been calculated
        if (!is.null(inverse)){
                # get it from the cache  
                message("getting cached data")
                return(inverse)
        }

        # else calculates the inverse 
        mat.data <- x$get()
        inverse <- solve(mat.data, ...)

        # sets the value of the inverse in the cache via the setinverse function.
        x$setinverse (inverse)

        inverse
}
