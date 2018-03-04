## Function for cathching inverse of matrix
## To reduce computation reputation it is beneficial to cache inverse of matrix

## The first function is used to create a matix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function creates inverse of the matrix created by makecacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
               message("getting cache data")
               return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
        

