## cacheMatrix applied after creating a special matrix
## with makeCacheMatrix returns the cached inverse of that matrix
## if it is available or calculates the inverse if there is no cached one


## makeCacheMatrix takes a matrix as an argument and returns the
## list of functions to: 1) set the value of the matrix; 2) get the
## value of the matrix; 3) set the value of the inverse (i.e. store
## it in cache); 4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {          ## stores the matrix in cache
            ## and resets its inverse
            x <<- y
            i <<- NULL
      }
      get <- function() x            ## returnes the initial matrix
      setinverse <- function(inverse) i <<- inverse  ## takes inverse from
      ## cacheSolve and stores it
      getinverse <- function() i     ## returns cached inverse
      ## the list of functions which is our 'special' matrix
      list(set = set, get = get,   
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve takes as an argument a list created with makeCacheMatrix function.
## It checks if the cached inverse is already in the list. If it is, then the
## cached inverse is returned; if it is not, the inverse is calculated and 
## stored in that list

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x':
      
      i <- x$getinverse()   ## Get the cached inverse i
      if(!is.null(i)) {     ## If i is not empty, then
            ## return the cached value and stop
            message("getting cached data")  
            return(i)
      }
      
      ## If there is no cached inverse (i is empty), find the inverse now:
      data <- x$get()       ## Access the initial matrix
      i <- solve(data, ...) ## Find the inverse
      x$setinverse(i)       ## Update the inverse in the cache
      i                     ## Return the inverse
}
