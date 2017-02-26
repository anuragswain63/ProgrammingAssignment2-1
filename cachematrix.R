## inverse matrix cache
## here we are going to create a matrix and then take its inverse and its cache
## if the matrix has a higher number of elements,computation time increases
## hence it is better to cache the inverse of the matrix
## well new to git so didnt commit earlier changes
## took me sometime to get a hang of this !!

## makeCacheMatrix is used to create a matrix
## set(),get() to set and obtain matrix values
## setinverse(),getinverse() to set and obtain inverse matrix values
## mat_actual is used to store matrix values globally, inverse_actual is used to store inverse matrix values globally
## mat_param is used for local matris o/p and inverse_param is used to store inverse matix values locally


makeCacheMatrix <- function(mat_actual = matrix()) {
  inverse_actual <- NULL
  set <- function(mat_param) {
    mat_actual <<- mat_param
    inverse_actual <<- NULL
  }
  get <- function() mat_actual
  setinverse <- function(inverse_param) inverse_actual <<- inverse_param
  getinverse <- function() inverse_actual
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is used to find inverse of a matrix 
## and if it already exists the it caches the inverse

cacheSolve <- function(mat_actual, ...) {
        
  
  inverse_actual <- mat_actual$getinverse()
  ## if inverse_actual is not null then we return the already calculated inverse_actual
  if(!is.null(inverse_actual)) {
    
    message("ooh it was cached !!")
    return(inverse_actual)
  }
  mat_data <- mat_actual$get()
  inverse_actual <- solve(mat_data, ...)
  mat_actual$setinverse(inverse_actual)
  message("just calculated it !!")
  inverse_actual
}
