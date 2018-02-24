## makeCacheMatrix creates an object which holds a matrix
## and its inverse

## cacheSolve computes the inverse of a matrix if it has not
## been already computed

## makeCacheMatrix function creates an object which stores a matrix,
## its inverse and functions to set and retrieve data

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    # initialize the matrix and the inverse
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    # return the matrix
    get <- function() x
    
    # set the inverse
    setinv <- function(inv) i <<- inv
    
    # return the inverse
    getinv <- function() i
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of the object returned
## by makeCacheMatrix. If the inverse has alredy been computed, then
## it is retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        
        # if i is not NULL, i.e., inverse has been calculated
        # then it is retrieved from cache
        if (!is.null(i)){
            message('Inverse is retrieved from cache')
            return(i)
        }
        
        # get matrix
        mtrx <- x$get()
        
        # compute the inverse
        i <- solve(mtrx, ...)
        
        # set the inverse
        x$setinv(i)
        
    return(i)
}
