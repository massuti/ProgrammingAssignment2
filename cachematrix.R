## Set matrix, get matrix set, solve(matrix), get solve(matrix)

## makeCacheMatrix clears "m", than store a matrix in x, than sets it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve check cache for the matrix on makeCacheMatrix

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setsolve(m)
        m
}