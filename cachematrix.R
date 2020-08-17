## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#this is the function that can create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) #set x as a matrix object {
m <- NULL 
  set <- function(y) { #set the value of the matrix 
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matrix 
  setinverse <- function(solve) m <<- inverse #set the value of the inverse
  getinverse <- function() m #get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}




## Write a short comment describing this function
#this is the function that computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) { m <- x$getinverse() 
  if (!is.null(m)) { #it first checks to see if the inverse has already been calculated
    message("getting cached data")
    return (m) #If so, it gets the inverse from the cache and skips the computation
  }
  data <- x$get() #Otherwise, it calculates the inverse of the data
  m <- solve(data, ...)
  x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
}
