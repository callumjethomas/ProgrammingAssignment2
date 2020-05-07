# This function will create a list of four functions:
        # set (sets the matrix from argument input and initialises m to NULL)
        # get (gets the stored matrix)
        # setSolve (sets s to be the solved inverse of the matrix (i.e. caches it))
        # getSolve (returns the cached inverse value stored in s)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
          x <<- y
          s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)

}

# This function will either return the cached s value or create a new s value to be cached
                # if s is not NULL then we must already have a cached value, so we simply return it
                # if s is NULL then we have nothing in the cache, therefore we need to:
                        # 1. get the new input matrix using get(), 
                        # 2. solve for the inverse 
                        # 3. cache this result using setSolve()
                        # 4. return the actual result (inverse of input x)

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
