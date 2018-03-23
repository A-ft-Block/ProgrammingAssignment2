## makeCacheMatrix
#  returns a list of getter and setter function callers
#  stores data to and retrieves data from cache

makeCacheMatrix <- function(cm = matrix()) {
  
  #initialise inverse matrix 
  im <- NULL
  
  #function argument 'cm' becomes function argument 'matrix_arg' for set function
  matrix_arg<-cm
  
  #define function caller "set"
  #assign value to cache matrix (cm) and inverse matrix (im)
  set <- function(matrix_arg){
    cm<<-matrix_arg
    #initialise inverse matrix
    im<<-NULL
  }
  
  #define function caller "get"
  #return value of cache matrix (cm)
  get<-function(){
    cm
  }
  
  #define function caller "setinverse"
  #save inverted matrix to cache
  setinverse<-function(inverse_arg){
    im<<-inverse_arg
  }
  
  #define function caller "getinverse"
  #return cached inverse matrix
  getinverse<-function(){
    im
  }
  
  #return a list of function callers
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheInverse
#  Returns the inverse matrix of 'm' either from cache or
#  calculated on the fly with inbuilt R function solve()
cacheInverse <- function(m, ...) {
  #create list 'x' of function callers with argument 'm' (matrix)
  x<-makeCacheMatrix(m)
  #check if inverse matrix is already in cache
  im<-x$getinverse()
  #if so, read inverse from cache
  if(!is.null(im)){
    message("getting cached data")
    #return inverse matrix from cache
    return(im)
  }
  #otherwise store value of matrix in temp var "tempM"
  tempM<-x$get()
  #calculate inverse on the fly with solve() function
  im<-solve(tempM)
  #store inverse matrix in cache
  x$setinverse(im)
  #return inverse matrix
  im
}
