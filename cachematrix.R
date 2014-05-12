## makeCacheMatrix creates a matrix object that cache its inverse
## cacheSolve computes the inverse of the matrix object created by the function makeCacheMatrix

##  makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y){
       x <<- y
       inverse <<- NULL
  }
  get <- function() x
  ## set and get methods for the inverse matrix
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function()inverse
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)

}


## cacheSolve function calculates the inverse of the matrix x
## if the inverse already exists then it is retrieved from the matrix with the getinverse function
## if the inverse not exists then it is calculated with the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix))   ## the inverse of the matrix already exists
  {
    message('getting cache data from the special matrix')
    return(inversematrix)
  }
  data<- x$get()      ## calculate the inverse of the matrix if not exists 
  inversematrix <-data %*% solve(data)
  x$setinverse(inversematrix)
  inversematrix
}

