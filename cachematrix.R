makeCacheMatrix <- function(x = numeric()) {
        solvedmatrix <- NULL
        set <- function(y) {
                x <<- y
                solvedmatrix <<- NULL
        }
        get <- function() x
        setsolvedmatrix <- function(solution2beCached) solvedmatrix <<- solution2beCached
        getsolvedmatrix <- function() solvedmatrix
        list(set = set, get = get,
             setsolvedmatrix = setsolvedmatrix,
             getsolvedmatrix = getsolvedmatrix)
}

cacheSolve <- function(x, ...) {
        solvedmatrix <- x$getsolvedmatrix()
        if(!is.null(solvedmatrix)) {
                message("getting cached data")
                return(solvedmatrix)
        }
        data <- x$get()
        solvedmatrix <- solve(data, ...)
        x$setsolvedmatrix(solvedmatrix)
        solvedmatrix
}