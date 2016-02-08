## Overall, the makeCacheMatrix function contains 4 other subfunctions including: Vector_Set, Vector_Get, Inverse_Cache, Inverse_Get which
## stores a matrix and a cached inverse version of the matrix, so that it can be later recalled via another function. 

makeCacheMatrix <- function(x=numeric()) {
  DataCache <- NULL
  Vector_Set <- function(DataInput) {
    x <<- DataInput
    DataCache <<- NULL
  }  
  Vector_Get <- function() {
    x
  }
  Inverse_Cache <- function(solve) {
    DataCache <<- solve
  }
  Inverse_Get <- function() {
    DataCache
  }
  list(Vector_Set = Vector_Set, Vector_Get = Vector_Get, Inverse_Cache = Inverse_Cache, Inverse_Get = Inverse_Get )
  }


## The cacheSolve function computes the inverse of the matrix of the makeCacheMatrix function. If the cache version is available, it
## will return it, otherwise it will process the computation and return the answer afterwards. 

cacheSolve <- function(y, ...) {
  Inverse <- y$Inverse_Get()
  if(!is.null(Inverse)) {
    return(Inverse)
  }
  Data <- y$Vector_Get()
  Inverse <- solve(Data)
  y$Inverse_Cache(Inverse_Cache)
  Inverse
}
