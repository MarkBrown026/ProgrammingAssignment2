
## This function saves computing time by cacheing the inverse of the Matrix
## This is accomplished by:
## 1. Setting the value of the matrix
## 2. Getting the value of the matrix
## 3. Setting the value of the matrix inverse
## 4. Getting the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y) {
      x <<- y
        inv <<- NULL
  }
   get <- function() x
     setinverse <- function(inverse) inv <<- inverse
       getinverse <- function() inv
           list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the matrix inverse in two steps
## First the function checks to see if the inverse has already been sovled
## If so then it skips the compuation and gives the result
## If the inverse has not been solved it computes and caches the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
          return(inv)
  }
  data <- x$get()
     inv <- solve(data)
         x$setinverse(inv)
             inv
  
  }
