## This function accepts an invertible matrix as an input, it returns a list of functions which can do the following:
## *set the value of the matrix
## *get the value of the matrix
## *set the value of the inverse of the matrix
## *get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverse) m <<- inverse
        getInverseMatrix <- function() m
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## This function returns the inverse of a matrix, if already present in the memory then it is fetched from there else is computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        ## If value already exists, then do not recompute but fetch from memory
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # If no stored value found, then fetch the value of the matrix
        data <- x$get()
        # Inverse the matrix
        m <- solve(data, ...)
        # Set the inverse value of the matrix
        x$setInverseMatrix(m)
        m
        
}


## Test the code
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
