## Programming Assignment 2 ---------------------------
## This pair of functions will cache the inverse of a matrix

## makeCacheMatrix will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## build the setter function
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## build the getter function
    get <- function() x
    
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    
    ## Return the four functions
    list(set = set, 
         get = get, 
         setInv = setInv,
         getInv = getInv)
}



## cacheSolve will compute the inverse of the special "matrix" returned by the function above
## If the inverse has already been calculated then cacheSolve should retrieve the inverse from cache

cacheSolve <- function(x, ...) {
    ## first initailize i by pulling it from the 'getInv' function passed in (from above)
    ## if this function has not already been run, i will be 'NULL'
    i <- x$getInv()

    ## if the inverse of x is stored in cache --> Return it from cache
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    ## otherwise, calculate the inverse of x and return
    data <- x$get()     ## first get the matrix called x and assign it var 'data'
    i <- solve(data, ...)    ## run 'solve()' in 'data' to get the inverse matrix & assign to 'i'
    x$setInv(i)         ## run setInv() to set the value of i to 'i' in the parent environment (cache)
    i                   ## return i
}
