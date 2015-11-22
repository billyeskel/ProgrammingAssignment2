## makeCacheMatrix and cacheSolve
## These functions work together to either 
## a. avoid recalc of the inverse of a matrix if it has already been stored in cache
## b. if not in cache then calc the inverse 

## To do this, the function makeCacheMatrix stores 4 functions in list
## 1. get (just returns the matrix x in the main function) 
## 2. set (overwrites x if there is a new matrix)
## 3 & 4, getmat & setmat (these just store the input into the variable called m)

## when 3 & 4 are used with casheSolve below they will either 
## a. calc a new inverse based on a new matrix
## b. return the inverse that was stored in cache 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmat <- function(solve) m <<- solve
        getmat <- function() m
        
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)
        
        }

## this function cacheSolve either
## a. gets the inverse as prevously stored in cache
## b. if there is a new matrix, vis a vis the variable data, then it will calc the inverse of the new matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmat(m)
        m
}
 