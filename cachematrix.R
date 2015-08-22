## Following function that can caching the inverse of a matrix rather than computing it 
## repeatedly. 

## this function  creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setsolve<-function(solve) inv<<-solve
  getsolve<-function() inv
  list(set=set,get=get,setsolve=setsolve,
       getsolve=getsolve)
}


## This function computes the inverse of special "matrix" returned by the makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getsolve()
  if(!is.null(inv)){
        message("getting cached data")
        return(inv)
  }
  dat1<-x$get()
  inv<-solve(dat1,...)
  x$setsolve(inv)
  inv
}
