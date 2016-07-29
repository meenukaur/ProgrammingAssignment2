## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMatrix creates a special matrix created by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()){
  inv <-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)inv<<-inverse
  getInverse<-function()inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## This function calculates the inverse of the special matrix created above. However, it first checks to see if the inverse has allready been calculated.If so,it gets the mean from the cache and skips calculation.

cacheSolve <- function(x, ...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
