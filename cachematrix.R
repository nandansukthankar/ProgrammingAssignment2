## makeCacheMatrix creates the special matrix that can cache its inverse
## cacheSolve is a function which calculates the inverse of the matrix and 
## caches it for further use. It extracts the inverse from the cache if it
## is already calculated before.
## Example: 
## 1. m <- matrix(c(3,5,6,4,5,8,9,0,2), 3, 3)
## 2. x <- makeCacheMatrix()
## 3. x$set(m)
## 4. cacheSolve(x)

## This function creates a special matrix and creates a list of functions 
## to be used for inverse operations on the matrix
## Four functions are created:
##      1. set - set the value of matrix y to matrix x
##      2. get - get the value of matrix x       
##      3. setinv - set the value of inv to be the inverse of matrix
##      4. getinv - get the value of inv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of a non-singular matrix, if not already
## calculated. Picks the value of inverse from cache if it was already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        print(data)
        inv <- solve(data)
        x$setinv(inv)
        inv
}
