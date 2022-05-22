## We have two functions that will allow to cache (temporarily store)
## the date and solve for the inverse of the matrix if it isn't
## already solved prior.

## makeCacheMatrix allows us to set and retrieve values for the matrix in an
## environment. This is the function that cacheSolve will be
## depending on.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}


## cacheSolve is responsible for retrieving the inverse value
## of the matrix, and if it isn't existing yet, it solves for it
## and then sets the value so that we may later retrieve it.

cacheSolve <- function(x, ...) {
  
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data...")
    return(i)
  }
  mtrx<-x$get()
  i<-solve(mtrx, ...)
  x$setinv(i)
  i
}

