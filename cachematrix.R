
## makeCacheMatrix function
## creates a vector of functions as follows
## on first run set will run to initialize the matrix
## get will return the matrix
## setinv will solve the matrix and store the inverse
## getinv will return the inverse if it was already solved

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #sets value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #gets value of matrix
  get <- function() x
  # sets value of inverse matrix
  setinv <- function(solve) inv <<- solve
  # gets inverse matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}



## cacheSolve function
## given a matrix it trys to retrieve the inverse if it is cached
## if the returned matrix is null it solves the matrix
## caches and returns the solution

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # if inverse was calculated (is not null) then return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) ##function call ends here returning the solution
  }
  ## if solution is null then get the matrix
  data <- x$get()
  ## solve the matrix
  inv <- solve(data, ...)
  ## cache the matrix and return solution
  x$setinv(inv)
  inv
}
