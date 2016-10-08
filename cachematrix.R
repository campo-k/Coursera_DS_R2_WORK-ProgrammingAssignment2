## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    chcekMatrix <- isInvertibleMatrix(x)
    if (!chcekMatrix) x <- NULL
    
    m <- NULL
    set <- function(y) {
        x <<- y
        chcekMatrix <- isInvertibleMatrix(y)
        if (!chcekMatrix) { x <<- NULL }
        m <<- NULL
    }
    get <- function() { x }
    setIV <- function(matrix) m <<- matrix
    getIV <- function() m
    list(set = set, get = get,
         setIV = setIV,
         getIV = getIV)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    data <- x$get()
    chcekMatrix <- isInvertibleMatrix(data)
    if (!chcekMatrix) {
        return(data)
    }
    m <- x$getIV()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    m <- solve(data, ...)
    x$setIV(m)
    m
}


## Add function for check Invertibl eMatrix

isInvertibleMatrix <- function(x) {
    if (!is.matrix(x)) {
        message("matrix type needed")
        return(FALSE)
    }
    
    rowCount = nrow(x)
    colCount = ncol(x)
    
    if (rowCount != colCount) {
        message("square matrix needed")
        message("rowCount: " ,rowCount , " colCount: ", colCount)
        return(FALSE)
    }
    else if (length(x[!x[] == 0]) == 0) {
        message("it's a zero matrix")
        return(FALSE)
    }
    else if (length(x[!x[] == x[1,1]]) == 0) {
        message("it's a zero matrix")
        return(FALSE)
    }
    else {
        return(TRUE)
    }
}