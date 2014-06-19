## Pair of functions. First creates a new object that is a matrix & can cache its inverse
## Second computes the inverse of the matrix, unless it has already been calculated. In that case use the cached version

## Write a short comment describing this function

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

cacheSolve <- function(x=matrix(), ...) {
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}