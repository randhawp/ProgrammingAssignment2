## Lexical scoping in R example

## makeCacheMatrix shows use of <<- operator to cache a matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
## cacheSolve takes a matrix and returns inverse
## if matrick is already in cache the cache is used
## usage
## source("cachematrix.R")
## mat<-makeCacheMatrix()
## mat$set(matrix(1:4,2,2))
## cacheSolve(a)

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
       return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}