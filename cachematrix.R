# makeCacheMatrix will return a set of functions for matrix manipulation.
# It will also cache the current matrix and the matrix inverse.
makeCacheMatrix <- function(var1 = matrix()) {
    set <- function(y) {
        matVar <<- y
        invmatVar <<- NULL
    }
    get <- function() matVar
    setMatrixInv <- function(inverseMatrix) invmatVar <<- inverseMatrix
    getMatrixInv <- function() invmatVar
    if (exists("matVar")) {
        if (!identical(matVar,var1)) {
            set(var1)
        }
    } else {
        set(var1)        
    }
    list(get = get,
         setMatrixInv = setMatrixInv,
         getMatrixInv = getMatrixInv)
}
# Cache solve function provides the inverse of a matrix.
cacheSolve <- function(matVar, ...) {
    invmatVar <- matVar$getMatrixInv()
    if(!is.null(invmatVar)) {
        message("Proceeding to get cached data")
        return(invmatVar)
    }
    #Get the matrix
    data <- matVar$get()
    # Get the inverse matrix using the solve function. 
    invmatVar <- solve(data)
    matVar$setMatrixInv(invmatVar)
    invmatVar
}