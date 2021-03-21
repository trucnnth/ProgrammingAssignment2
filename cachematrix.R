## These 2 create a function list, components of which would be used to calculate and cache a matrix's inverse

## Create a list of functions to be called to calculate the inverse of matrix x

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverseMatrix) inv <- inverseMatrix
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## Check if inverse inv already exist. If not, calculate, set result as inv and return inv

cacheSolve <- function(x, ...){
    inv <- x$getInv()
    if (!is.null(inv)){
        message('getting cache data')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv()
    inv
}