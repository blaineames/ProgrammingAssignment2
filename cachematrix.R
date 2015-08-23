##This function takes in a matrix as an argument.
##The function then sets internal objects that are initially empty
##and creates a list that is a function that solves

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    set_mat <- function(solve) m <<- solve
    get_mat <- function () m
    list(set = set, get=get, 
            set_mat = set_mat,
            get_mat = get_mat)
  
}


## This function looks at the cached matrix and decides whether or not 
## the matrix needs to be solved for or not. 

cacheSolve <- function(x=matrix(), ...) {
  m <- x$get_mat
  if(!is.null(m)) {
    message("getting cached matrix")  ##if the inverse matrix is cached this statement is executed
    return(m)                         ##and the cahced matrix is returned
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_mat(m)
  m
}
