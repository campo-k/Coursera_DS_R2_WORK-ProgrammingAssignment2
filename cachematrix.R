## this is function for store matrix and calculate inverse matrixs

## stored a matrix and inverse matrix
## check argument what invertible matrix 

makeCacheMatrix <- function(x = matrix()) {
    checkMatrix <- isInvertibleMatrix(x)
    if (!checkMatrix) x <- NULL
    
    m <- NULL
    set <- function(y) {
        x <<- y
        checkMatrix <- isInvertibleMatrix(y)
        if (!checkMatrix) { x <<- NULL }
        m <<- NULL
    }
    get <- function() { x }
    setIV <- function(matrix) m <<- matrix
    getIV <- function() m
    list(set = set, get = get,
         setIV = setIV,
         getIV = getIV)
}


## calculate inverse matrix
## if arg x has a inverse matrix, return message
## if raf x isn't invertible matrix, return messge

cacheSolve <- function(x, ...) {
    data <- x$get()
    checkMatrix <- isInvertibleMatrix(data)
    if (!checkMatrix) {
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