## These functions simplify getting an inverse of a matrix when the matrix
## does no9t change, by caching the inverse for easy retrieval. If the inverse
## has not already been cached, then it calculates it and the caches it.

## makeCacheMatrix is a function that creates a special"matrix" object 
## that can cache its inverse.It essentially creates a list that,sets 
## the value of the matrix, gets the value of the matrix, sets the 
## value of the inverse of the matrix, and gets the value of the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y){
  x<<-y
  i<<-NULL}
get<-function()x
setsolve<-function(solve)i<<-solve
getsolve<-function()i
list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve is a function that checks if the inverse has already been
## calculated and cached (and the matrix has not changed). If it has been 
## cached then it retrieves the inverse. Otherwise it calculates the 
## inverse from the data and sets it in the cache via the setsolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getsolve()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setsolve(i)
  i
}
