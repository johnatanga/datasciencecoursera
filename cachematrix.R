## The function below is used to check the cache for inverse  of matrix
## If not found then one should compute the inverse using solve()

## The function uses Lexical scoping
## It should return  a list of four functions set, get, setinverse and get inverse.

makeCacheMatrix <-  function(x = matrix()) {
  
  i <-  NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
    
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  Thus function caculates the inverse of the matrix but first checks cache should there be a code

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if  (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}