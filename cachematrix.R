

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  d <- det(x)    ## determines the determinant of the matrix
  
  ## If the determinant of the matrix is equal to zero, notifies the user that 
  ## the given matrix is singular and non-invertible
  if(d == 0){
    message ("Matrix is singular, non-invertible")
  }
  
  
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function () i
  list( set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
  

}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  i <-x$getinverse()
  if(!is.null(i)){     
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  return(i)
}
