## This code permits the storage and retrieval of a matrix along with the
## calculation and caching of the inverse of the stored matrix
## Student: Jim McCusker
## Email: jmccusker@gmail.com

## This function stores & retrieves the matrix and allows for the setting and 
## retrieval of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL   ## initialize inverse matrix (im) as null to indicate it hasn't been calculated
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inv) im<<-inv
  getinverse <- function() im
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im<-x$getinverse()
  if(!is.null(im)) {
    message("returning cached inverse matrix")
    return(im)
  }
  mat<-x$get()
  im<-solve(mat)
  x$setinverse(im)
  im
}
