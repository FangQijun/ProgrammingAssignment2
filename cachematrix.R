## The two functions here are for calculating the inverse of a 
## matrix. What is special here  is that, if the inverse of the 
## matrix is calculated once before, the functions directly 
## read the result from cache instead of recalculating.

## The makeCacheMatrix() funtion here creates an artificial list 
## including a function to: I.set the value of  the matrix; II. 
## get the value of the matrix; III. set the value of the inverse; 
## IV. get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                ##Set an empty value to 'm'
  set <- function(y) {
    x <<- y                ## Assign the value 'y' to the R object
                           ## 'x' which is in its own environment, 
                           ## one that is different from the current
                           ## one ("<<-"s below are likewise).
    m <<- NULL
  }
  get <- function() x      ## Assign function(x) to 'get'
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m  ## Assign function(m) to 'getinverse'
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ## Lastly, return four functions
}

## The cacheSolve() function here calculates the inverse of 
## the list of 4 functions generated from the makeCacheMatrix() 
## function. Specifically, it first does some checking. If the 
## inverse has been calculated, it copies the result from the
## cache memory and drops calculation.If not yet, it calculates 
## the inverse of the matrix typed in and sets the solve() value 
## in the cache with the setinverse function.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {          ## if the cache memory is not NULL,
    message("getting cached data")  ## to expedite the caculation,
    return(m)                ## we return the value stored in cache
  }                          ## without any computation.
  
  data <- x$get()            ## if there is not any values stored
  m <- solve(data, ...)      ## in cache, we calculate the inverse
  x$setinverse(m)            ## using the solve() function in R.
  m                ## Return a matrix that is the inverse of 'x'
}
