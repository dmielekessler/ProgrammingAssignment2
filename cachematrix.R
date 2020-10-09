## For Coursera < R Programming < Week 3 < Programming Assignment 2: Lexical Scoping 
## This pair of functions stores a matrix inverse as part of a list in cache or retrieves it from a list in cache


## This first function creates an object (list) of type makeCacheMatrix including a marker indicating the matrix inverse has not yet been solved and cached
makeCacheMatrix <- function(x = matrix()) {
  marker <- NULL
  set <- function(placeholder) {
    x <<- placeholder
    marker <<- NULL
  }
  get <- function() x
  setinverse <- function(makeinverse) marker <<- makeinverse
  getinverse <- function() marker
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a 'matrix' of type makeCacheMatrix, retrieve the inverse from cache, or if it doesn't exist, solve for the inverse
## This function only works on items of type makeCacheMatrix, which is a list of items, one of which is a placeholder for the inverse

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        ## This function checks to see if the object (list) of type makeCacheMatrix includes a marker indicating the matrix inverse has been solved and cached
        ## If the marker exists, the function retrieves the matrix inverse from cache
        ## Otherwise (not specified as an 'else', the function just skips the 'if'), the function solves for the inverse
        ## and then stores the inverse as the marker in object (list) of type makeCacheMatrix
    marker <- x$getinverse()
    if(!is.null(marker)) {
      message("getting cached data")
      return(marker)
    }
    data <- x$get()
    marker <- solve(data, ...)
    x$setinverse(marker)
    marker
  }
