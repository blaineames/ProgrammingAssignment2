## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
  m <- x$get_mat
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_mat(m)
  m
}
