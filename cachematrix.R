## Part 1 of the assignment. This function creates a matrix and compute the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(sol) inv <<- sol
  getinv <- function() inv
  list(set=set,get=get,
       setinv=setinv, getinv=getinv)

}


## This function checks for the cached inverse and in case it is not found, it uses solve()
## to compute the inverse and set back the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("Fetching Cached Data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
