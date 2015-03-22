makeCacheMatrix <- function(a = matrix()) {
  i <- NULL               ##set inverse of a matrix to null

  ## set matrix
  set <- function(b) {
    a <<- b
    i <<- NULL
  }
  ## get matrix
  get <- function() a           
  ## setting inverse matrix
  setimatrix <- function(imatrix) 
    i <<- imatrix
  ## getting inverse matrix
  getimatrix <- function() i                 
  
  ## list names
  list(set = set, get = get,
       setimatrix = setimatrix,
       getimatrix = getimatrix)
}


## cacheSolve computes the inverse of the matrix 
cacheSolve <- function(a, ...) {
  
  ## imatrix value
  i <- a$getimatrix() ## gets the invmat in previous function
  
  ## if inverse matrix is already stored, get cached data and return the inv matrix
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if not, calculate inverse of the matrix 
  data <- a$get()              ## get the matrix
  i <- solve(data, ...)      ## calculate the inverse
  a$setimatrix(i)             ## set the inverse of the matrix
  i                         ## print the inverse of the matrix

}

