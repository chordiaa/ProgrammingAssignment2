## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.
## The function returns a list having the follwing functions:
## 1. set: This function sets the value passed to it as a matrix and sets its 
##         inverse as NULL(empty value)
## 2. get: This function returns the matrix 
## 3. setinv: This function sets the value passed as input to it as the inverse
##            of the matrix
## 4. getinv: This function returns the inverse of the matrix. 
##            If the inverse of the matrix was set using the setinv function,  
##            then this function will return the inverse matrix


makeCacheMatrix <- function(mtx = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get,
                    setinv = setinv,
                    getinv = getinv))
}


## The cacheSolve function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## this function retrieves the inverse from the cache.

cacheSolve <- function(mtx, ...) {
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
                ## Get Cached data
                return(inverse)
        }
        data <- mtx$get()
        inverse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}

