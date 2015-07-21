###################################################################
#title: 'R Programming: Programming Assignment 2: Lexical Scoping'
#author: "Matthew Fergusson"
#date: "July 21, 2015"
#Purpose: Create a function to find the inverse of a matrix or 
          #use a cached value when available
###################################################################

## This function creates a special "matrix" object that can cache its inverse.

#create matrix for testing
MTF1 <- matrix(c(-3,2,-3,-1,1,3,2,0,-1),3,3)
  #QC check
  class(MTF1)
  #find the determinant of the matrix 
    #(if zero or close to zero then there is no inverse and an error message will be produced)
  det(MTF1)
  #create inverst to the matrix
  solve(MTF1)
  #prove that it is the inverse (or very close)
  MTF1 %*% solve(MTF1)

#Create makeCacheMatrix() functions
  ## This function stores a"matrix" entered as the input and .
  ## If the inverse has already been calculated (without changing the matrix), then cacheSolve 
  ## retrieves the inverse from the cache instead of re-calculating it.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    #Use '<<-' to assign value outside of current environment
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setmatrix <- function(solve) m <<- solve 
  getmatrix <- function() m
  
  #create list to record cached values for the cacheSolve() function
  list(set = set
       , get = get
       , setmatrix = setmatrix
       , getmatrix = getmatrix)
}

#run makeCacheMatrix()
MTF2 <- makeCacheMatrix(MTF1)

#Create cacheSolve() functions
  ## This function finds the inverse of the "matrix" entered into the makeCacheMatrix function above.
  ## If the inverse has already been calculated (without changing the matrix), then cacheSolve 
  ## retrieves the inverse from the cache instead of re-calculating it.
cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix() 

  #if x$getmatrix() is not null then use cached value without re-running
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }

  #if x$getmatrix() is null then calculate the inverse of the original input matrix from makeCacheMatrix()
  matrix <- x$get()
  m <- solve(matrix)
  x$setmatrix(m)
  
  #return the inverse of the original input matrix from makeCacheMatrix()
  return(m)
}

MTF3 <- cacheSolve(MTF2)

#QC check
MTF1 %*% MTF3
  #approximately identity matrix