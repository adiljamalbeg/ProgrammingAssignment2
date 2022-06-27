## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function(){
    
    i<-ginv(x)
    i%*%x
    
  }
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$geti()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}



#Test
# t <-makeCacheMatrix(matrix(1:8,2,4))
# 
# t$get()
# 
# t$geti()
# 
# cacheSolve(t)

