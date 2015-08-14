
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #sets m to NULL
  set<-function(y){  #sets value of the matrix
    x<<-y  #caches the input matrix
    m<<-NULL
  }
  get<-function() x 
  setmatrix<-function(solve) m<<- solve 
  getmatrix<-function() m 
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix() #gets m from x
  if(!is.null(m)){
    message("getting cached data") #if m!=NULL,sends a text message and returns the cached matrix
    return(m)
  }
  matrix<-x$get() #gets value from matrix
  m<-solve(matrix, ...) # computes inverse matrix
  x$setmatrix(m) #caches inverse matrix
  m #returns inverse matrix
}