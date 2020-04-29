## Creating the "set" and "get" list
makeCacheMAtrix <- function(x = matrix()){
  mInversed <- NULL
  set <- function(y){
    x <<- y
    mInversed <<- NULL
  }
  get <- function() x
  setInverser <- function(Inverser) mInversed <<- inverser
  getInverser <- function() mInversed
  list(set = set, get = get, setInverser = setInverser, getInverser = getInverser)
}
##  function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated,
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x,...){
  mInversed <- x$getInverse()
  if(!is.null(mInversed)){
    message("getting cached data")
    return(mInversed)
  }
  d <- x$get()
  ## ## function mInversed creates a special matrix object that can cache its inverse
  mInversed <- qr.solve(d) %*% d
  x$setInverser(mInversed)
  mInversed
}

