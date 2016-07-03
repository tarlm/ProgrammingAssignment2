## MakeCacheMatrix: cache a matrix object and contrains a list of functions use to
## set or get matrix or its inverse matrix
## cacheSolve: calculate the inverse matrix and cached it

## Set the cached inverse matrix if matrix changed

makeCacheMatrix <- function(x = matrix()) {
    
    matrix_invers <- NULL
    set <- function(y) {
        x <<- y
        matrix_invers <<- NULL
    }
    
    get <- function() x
    setmatrixInverse <- function(matrix) matrix_invers <<- matrix
    getmatrixInverse <- function() matrix_invers
    list(set = set, get = get,
         setmatrixInverse = setmatrixInverse,
         getmatrixInverse = getmatrixInverse)
}


## Returns inverse matrix if exists in cache or calculate it if not. It take matrix object in parameter

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix_invers <- x$getmatrixInverse()
    if(!is.null(matrix_invers)) {
        message("getting cached data")
        return(matrix_invers)
    }
    
    data <- x$get()
    matrix_invers <- solve(data, ...)
    x$setmatrixInverse(matrix_invers)
    matrix_invers
}

###################### Test Space ##############
# my_matrix <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
# matObject <- makeCacheMatrix(mat)

# Get Matrix and matrix inverse from object
# matObject$get()
# matObject$getmatrixInverse() # NULL

# run cache function 
# cacheSolve(matObject)

# Get Matrix and matrix inverse from object
# matObject$get()
# matObject$getmatrixInverse() # has not null value now