
makeCacheMatrix <- function(x = matrix()) {
  # creates 4 functions which are available to cacheSolve by leveraging 
  # The rules of lexical scoping.
  # The ->> operator is used to assign a value to an object 
  # in an environment different than the current environment
  # Returns a list containing 4 functions
  #   1. set matrix
  #   2. get matrix
  #   3. set inverse
  #   4. get inverse
  #         
  
  i = NULL
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) i <<- inverse 
  getinv = function() i
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}

cacheSolve <- function(x, ...) {
 
  ## calculates and returns the inverse of the matrix passed to makeCacheMatrix()
  
  i = x$getinv()
  
  # if inverse is in cache use it instead of recalculating
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  # logic functions like else if.  If !is.null = FALSE execution continues at line below
  data = x$get()
  i = solve(data, ...)
  
  # setinv assigns the the inverse of the matrix to the cache 
  x$setinv(i)
  return(i)
  
  ## Code used to test
  #source("assign_2v2.R")
  #input_matrix = matrix(c(2,4,3,1,5,7,8,9,10),nrow=3,ncol=3)
  #a <- makeCacheMatrix(input_matrix)
  #b = cacheSolve(a)
  
}