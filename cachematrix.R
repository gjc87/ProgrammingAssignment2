## This script creates two functions that can be used together to caluclate the inverse of a matrix and store  
## that inverse matrix. This allows the inverse matrix to be used without recalculating it every time. 

## This function takes in a normal matrix as a variable and creates a unique matrix that can be used with other functions created in this script.
## It also defines two functions that set or get the value of the matrix, and
## two functions that set or get the inverse of the matrix to be used in the cacheSolve function following it. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function takes in the previously created matrix and first checks if the inverse is stored in the x$getinv() variable. If it is then the
## already calculated inverse will be returned (will never happen the first time it is run). If the value is null (not yet stored) then the
## if statment is skiped and the matrix is stored into 'data', the inverse is calculated with the solve() function, and the previously 
## defined setinv function is called to store the inverse with the use of <<-  

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## Test code I used:
## g <- makeCacheMatrix(matrix(c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3))
## cacheSolve(g)