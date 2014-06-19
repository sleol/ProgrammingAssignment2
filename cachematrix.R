## Pair of functions. First creates a new object that is a matrix & can cache its inverse
## Second computes the inverse of the matrix, unless it has already been calculated. In that case use the cached version

## makecachematrix  creates a list containing functions to:
## set the value of the matrix
## get the value of the matrix
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


## Cachesolve uses the solve() function to calculate the inverse of the matrix,
## first it checks if there is a cached version of the matrix to use instead.
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