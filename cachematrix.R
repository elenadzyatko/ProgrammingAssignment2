
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #sets m to NULL
  set<-function(y){  #sets value of the matrix
    x<<-y  #caches the input matrix
    m<<-NULL
  }
  get<-function() x #returns matrix
  setinverse<-function(inverse) m<<- inverse #saves inverse matrix to cache
  getinverse<-function() m #returns inverse matrix
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

cacheSolve <- function(x) {
  m<-x$getinverse() #gets m from x
  if(!is.null(m)){
    message("getting cached data") #if m!=NULL,sends a text message and returns the cached matrix
    return(m)
  }
  matrix<-x$get() #gets value from matrix
  m<-solve(matrix) # computes inverse matrix
  x$setinverse(m) #caches inverse matrix
  m #returns inverse matrix
}