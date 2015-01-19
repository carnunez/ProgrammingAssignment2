## The first function is used to set up a cache variable = inv by using (<<-). It returns a list of
## four functions
## The second function uses three functions of these four (getinverse, get and setinverse). 
##     * The first one is to check whether "inv" was previously calculated. If TRUE, then return
##       the cached value.
##     * Otherwise uses function (get) to get matrix data and (setinverse) to calculate "inv" and
##       cache its value


## FUNCTION1: Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
## it works also if only three functions kept in list vs. the four commented out below  
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
#  list(set = set, get = get,
#       setinverse = setinverse,
#       getinverse = getinverse)
}
## *************************************************************************************************
## FUNCTION2: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
## *************************************************************************************************
## TEST:

#> a<-matrix(1:4,2,2)
#> a
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> b<-makeCacheMatrix(matrix(1:4,2,2))
#> cacheSolve(b)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(b)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
