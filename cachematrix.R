## Put comments here that give an overall description of what your
## functions do

## This function creates a new matrix as specified in argument, stores itin x
## and stores NULL in obejct m; cachesolve function calls setsolve funxtion to
## store the inverse of matrix in m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # In theory a check can me made here to see if m is identicle to x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve  
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  

}


## This functions checks whether m stored in above function is null or not
## and based on it eithers retuns cached m or recalculates inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    # An additional check can be made here to compare x and m
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)  # Inverse of a square matrix
  x$setsolve(m)
  m
}

# Example of a test (from discussion forum)
#> source ("makeCacheMatrix.R")
#> source("cacheSolve.R")
#> x <- matrix(c(4,2,7,6), nrow=2, ncol=2)
#> y <- makeCacheMatrix(x)
#> z <- cacheSolve(y)
#> z
#[,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4
#> z <- cacheSolve(y)
#getting cached data
