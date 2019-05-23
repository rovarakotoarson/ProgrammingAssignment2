## Those 2 functions compute the inverse of a matrix,
## creating a special object (matrix) and begin to cache the inverse
## If it is already computed it shows the result, else it computes and print.

## This fuction sets the value of the matrix, gets its,
##  sets the inverse and gets it

makeCacheMatrix <- function(x = matrix()) {
  invm<-NULL
  set<-function(y){
    x<<-y
    invm<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) invm<<-solve
  getinverse<-function() invm
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix if it is not done 
##  yet and prints the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm<- x$getinverse()
  if (is.null(invm)){
    message("getting cached data")
    return(invm)
  }
  data<-x$get()
  invm<-solve(data,...)
  x$setinverse(invm)
  invm
}
