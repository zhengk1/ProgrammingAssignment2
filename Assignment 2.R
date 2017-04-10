#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  inverse<-NULL
  #set the value of the matrix
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  #get the value of matrix
  get<-function() x
  #set the value of inverse matrix
  setInverse<-function(inverse1) inverse<<-inverse1
  #get the value of inverse matrix
  getInverse<-function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x,...){
  inverse <-x$getInverse()
  #if the value of inver is not null, returns the cache data
  if (!is.null(inverse)){
    message("getting cache data")
    return(inverse)
  }
  
  #calls the function get
  matrix <- x$get()
  inverse <- solve(matrix,...)
  #sets the value of the inverse matrix
  x$setInverse(inverse)
  inverse
}

#Test
invMatrix <-makeCacheMatrix(matrix(c(1,2,5,7),2,2))
invMatrix$get()
#> mymatrix$get()
#     [,1] [,2]
#[1,]    1    5
#[2,]    2    7
cacheSolve(invMatrix) 
#> cacheSolve(invMatrix)
#           [,1]       [,2]
#[1,] -2.3333333  1.6666667
#[2,]  0.6666667 -0.3333333
#> cacheSolve(invMatrix)
#getting cache data
#           [,1]       [,2]
#[1,] -2.3333333  1.6666667
#[2,]  0.6666667 -0.3333333
