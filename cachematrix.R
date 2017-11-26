## Put comments here that give an overall description of what your
## functions do
#The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve().
#The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its invers
#The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve the inverse
#from the cached value that is stored in the makeCacheMatrix() object's environment

## Write a short comment describing this function:
#x and inv was initialized as a function arguments.the code provides four
#basic behaviors called getters and settters. set() assign the input argument to the x
#object and Null to inv object in the parent environment.
#get() retrieves x, inv from the parent environment.
#setinv set the value to inv(in parent environment) after solve is finished
#getinv get the right value of inv from the parent environment
# finally all of these functions are assigned ot list that will be returned to the
#parent environment
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
  
}


## Write a short comment describing this function
#cacheSolve retrieves the inverse of a matrix from an object of type makeCacheMatrix()
# first retrive the value of inv from the parent environment.
#check if the inverse was calculated before, if true then it return it
# if not then the x is retrived and the inverse is calculated and its value
# is set to the inv in the parent environment and return the results inv.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
