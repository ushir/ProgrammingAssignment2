## These functions creates a matrix, caches the matrix using lexical
## scoping rules and returns the inverse.  If repeatedly used without
## changes in original matrix, will not compute the inverse again.


## This function creates a new matrix as specified in argument, stores the  
## inverse in m when $setsolve is called and provides setsolve and getsolve
## methods to store and extarct information.
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
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

cacheSolve <- function(x,newm ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  oldm <- x$get()
  if(!is.null(m)) {
    # An additional compare to check the potential changes in matrix data
    if (identical(oldm,newm)) {
      message("getting cached data")
      return(m)
    }
  }

  m <- solve(newm,...)  # Inverse of a square matrix
  x$setsolve(m)
  m
}

# Example of a test (from discussion forum)
#> source ("makeCacheMatrix.R")
#> source("cacheSolve.R")
#> x <- matrix(c(4,2,7,6), nrow=2, ncol=2)
#> y <- makeCacheMatrix(x)
#> z <- cacheSolve(y,x)
#> z
#[,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4
#> z <- cacheSolve(y,x)
#getting cached data
#> x[1,1]<-100
#> z <- cacheSolve(y,x)
#> z
#[,1]        [,2]
#[1,]  0.010238908 -0.01194539
#[2,] -0.003412969  0.17064846
