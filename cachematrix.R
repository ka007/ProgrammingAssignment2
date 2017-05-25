# Functions below do Matrix inversion which is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
# We assume that the matrix supplied is always invertible.
#   
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# It comprises of 4 sub functions get, set getinv, setinv

makeCacheMatrix <- function(x = matrix()) {
  #initialise inv to NULL
  inv<-NULL
  
  #set is usefull if you want to set the current to a new matrix without calling the makeCacheMatrix,this will reset its inverse to null
  set<-function(y){
        x<<-y
        inv<<-NULL
  }
  
  #to get the current matrix which is set
  get<-function() x
  
  #this function sets the value of inverse
  setinv <-function(inver) inv<<-inver
  
  #gets the inverse which is currently set
  getinv <-function() inv
  
  #named list which helps in calling the functions 
  list(set =set, get = get, setinv =setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
## We use the solve function in R to calculate the inverse of the matrix
## As above we assume matrix supplied is square and invertible

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
              message("getting cached data")
              return(inv)
        }
        data <-x$get()
        if(det(data)==0){
          message("matrix is singular cannot be inversed")
          return()
        }
        inv <-solve(data,...)
        x$setinv(inv)
        inv
}
