## Put comments here that give an overall description of what your
## functions do

## This function creates the "getters" and "setters" for the matrix, to be used in cache solve

makeCacheMatrix <- function(x = matrix()) {
              MatInv <- NULL
              set <- function(y){
                x <<- y
                m <<- NULL
              }
              get <- function() x
              setMatInv <- function(Inv) MatInv <<- Inv
              getMatInv <- function() MatInv
              list(set = set, get = get,
                   setMatInv = setMatInv,
                   getMatInv = getMatInv)
}


## This function attemps to retrieve the Matrix Inverse from the cache; if not stored it will calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  MatInv <- x$getMatInv()
  if(!is.null(MatInv)) {
    message("getting cached data")
    return(MatInv)
  }
  data <- x$get()
  MatInv <- solve(data)
  x$setMatInv(MatInv)
  MatInv
}
