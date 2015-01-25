## The two functions 'makeCacheMatrix' and 'cacheSolve' collectively
## calculate and cache the inverse of a matrix, x

## This function takes a matrix (x) as an arument and returns a list
## of four functions, 'set', 'get', 'setInvMatrix', and 'getInvMatrix'

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(InvMatrix) m <<- InvMatrix
        getInvMatrix <- function() m
        list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## 'cacheSolve' is a 'wrapper' that takes the list output of makeCacheMatrix
## and uses it to calculate the inverse of matrix 'x' or, if the inverse has
## already been cached, returns the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInvMatrix(m)
        m
}
