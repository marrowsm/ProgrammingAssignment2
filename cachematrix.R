## Below are two functions that help you calculate the inverse of a matrix
# The first function creates a cache and then second one uses it during the actually computation

# Test data:
# c=rbind(c(1, -1/4), c(-1/4, 1)) 


## The first function here can be used to create a version of a matrix
## that has elements attached to it (such as it's inverse).

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <- NULL
  }
  get <- function() x
  setinv <- function(solve) inverse <<- solve
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function caculates the inverse of a matrix passed to 
## it, first by looking up whether or not the inverse exists in cache, and then,  
## should that fail, it calculates the inverse and stores it in cache for next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
  
}
