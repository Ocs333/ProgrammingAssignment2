## This pair of functions makes us able to cache the inverse of a matrix,
## which is quite useful, as taking the inverse of a large matrix is slow,
## and uses a lot of computation power
## After the two functions needed for this task an example shows how we
## can use these functions.
## The functions are based on the code of J.Peng

## The first function transforms our matrix into an object, which is
## in fact a list of four functions, which in order:
#     set the value of the original matrix
#     get the value of the original matrix
#     set the value of the inverse matrix
#     get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## After using the first function to make a cached matrix
## (Which will contain its inverse), we can use our second function to
## get its inverse instantly, or if it not calculated yet
## caches the inverse.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      ## If the inveres 'm' already cached, the function will notify us
      if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
      }
      ## Otherwise it calculates the inverse
      mat1 <- x$get()
      m <- solve(mat1, ...)
      x$setinverse(m)
      m
}

##########
#Example:
A<-matrix(1:4,2,2)
cA<-makeCacheMatrix(A)
AI<-cacheSolve(cA)
AI<-cacheSolve(cA)
# We get the notation that the cached inverse is being used
AI %*% A
#we get back the identity matrix
