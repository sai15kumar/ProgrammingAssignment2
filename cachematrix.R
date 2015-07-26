## This combination of functions cache a inverse of matrix and return them without need of recomputing again


## makeCacheMatrix stores/caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <-function(y){
    x <<-y
    m <<-NULL
  }
  get <-function() x
  setinv <-function(inv) m <<-inv
  getinv <-function() m
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## Checks if Cached matrix from makeCacheMatrix is the inverse and Computes inverse matrix if not so

cacheSolve <- function(x, ...){
        ## Returns a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
      print("getting cached data")
      return(m)
  }
  data <-x$get()
  m = solve(data,...)
  x$setinv(m)
  return(m)
}
