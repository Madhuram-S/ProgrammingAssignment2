## Apr,8,017- MS - Cache program to cache time consuming computation like inverse as done here
## the first program "makeCacheMatrix - creates a spl matrix to capture inverse
## second program "cacheSolve" - finds out the inverse of the cahced matrix

## Function to create spl matrices to capture a inverse

makeCacheMatrix <- function(orig_m = matrix()) {
  inv_m <- NULL ## initialize cache matrix to NULL
  set <- function(new_m){
    orig_m <<- new_m
    inv_m <<- NULL
  }
  
  get <- function() orig_m
  
  setInv <- function(solve) inv_m <<- solve
  getInv <- function() inv_m
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function calculates the inverse of a matrix atstnd stores it in cache if it not already cached

cacheSolve <- function(orig_m, ...) {
  ## Return a matrix that is the inverse of original matrix
  inv_m <- orig_m$getInv()
  
  if(!is.null(inv_m)){
    message("printing inverse matrix from cache")
    return(inv_m)
  }
  
  ## solve the inverse of the matrix
  mx <- orig_m$get()
  inv_m <- solve(mx)
  orig_m$setInv(inv_m)
  inv_m
}
