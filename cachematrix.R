makeCacheMatrix <- function(x = matrix()) {
  inverse <-NULL ## set the inverse to null for future values
  set<-function(y){ 
  x<<-y
  m<<-NULL
} ## define a function which takes y as a parameter; 
## y takes the matrix "x" and sets it to a new matrix "y" 
## it then resets the inverse to null as a placeholder for the future value of "y"

get<-function() x ## returns the matrix x
setmatrix<-function(solve) inverse <<- solve ## sets the inverse to solve
getmatrix<-function() inverse ## returns the inverse 
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
} 

cacheSolve <- function(x=matrix(), ...) {
    inverse<-x$getmatrix()
    if(!is.null(inverse)){
      message("getting cached data")
      return(inverse)
    }
    matrix<-x$get()
    inverse<-solve(matrix, ...)
    x$setmatrix(inverse)
    inverse
}
