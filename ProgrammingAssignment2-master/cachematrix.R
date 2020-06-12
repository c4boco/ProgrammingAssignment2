## Put comments here that give an overall description of what your
## functions do
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(matrix) {
                x <<- matrix
                i <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse){
                i <<- inverse
        }
        getinverse <- function(){
                i
        }
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x <- x$getinverse()
        if( !is.null(m)){
                message("getting data")
                return(x)
        }
        data <- x$get()
        
        x <- solve(data) %*% data
        x <-setinverse(x)
        x
}
