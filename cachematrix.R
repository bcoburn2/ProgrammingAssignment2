## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y #Sets/replaces the matrix
            s <<- NULL #resets the cache to null
      }
      get <- function() x
      setcache<- function(solve) s <<- solve # Sets the inverted matrix in the cache
      getcache <- function() s #calls the inverted matrix
      list(set = set, get = get, 
           setcache = setcache,
           getcache = getcache) #creates the list of functions that can be called
}


## This function computes or retrieves the inverse of the special 
## "matrix" returned by `makeCacheMatrix` above. If the inverse has

cacheSolve <- function(x, ...) {
      s <- x$getInverse()  #Attempts to retrieve the inverse matrix
      if(!is.null(s)) { #If the inverse isn't NULL it's retrieved
            message("getting cached data") 
            return(s)
      }
      data <- x$get() #not clear if this runs anyway. Seems redundant if it does
      s <- solve(data, ...)
      x$setcache(s)
      s
}