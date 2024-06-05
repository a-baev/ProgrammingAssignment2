## Writing an R function that is able to cache potentially time-consuming computations
## For example, taking the mean of a numeric vector is typically a fast operation. 
## However, for a very long vector, it may take too long to compute the mean, 
## especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a vector are not changing, 
## it may make sense to cache the value of the mean so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 


## The following makeCacheMatrix function returns 
## the list of functions set, get, set_inv and get_inv 
## to set/get either matrix or inverse matrix


makeCacheMatrix <- function(x=matrix()) {
  inv_matr <- x
  set <- function(y){
      x <<- y
      inv_matr <<- NULL
      }
  get <- function() x
  set_inv <- function(inv_m) inv_matr <<- inv_m
  get_inv <- function() inv_matr
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## The cacheSolve function gets the list of functions (the output of makeCacheMatrix function),
## checks if inverse matrix exists in cache.
## if it does, it returns inverse matrix
## id it doesn't, it calculates and returns inverse matrix

cacheSolve <- function(x) {
  inv <- x$get_inv()
  if (!is.null(inv)) {
    message("getting inversed matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inv(inv)
  inv
}


###### EXAMPLE OF UTILIZATION OF DEVELOPED FUNCTIONS

my_matr = matrix(c(1,2,3,4,5,7,8,9,12), 3,3)
my_matr

list = makeCacheMatrix()
list

list$get()
list$get_inv()

list$set(my_matr)
list$get()
list$get_inv()

cacheSolve(list)
list$get()
list$get_inv()

list$set_inv <- cacheSolve(list)
list$get_inv()

my_matr2 = matrix(c(-3,3,-1,8,-12,5,-4,7,-3), 3,3)
list$set(my_matr2)
list$get()
list$get_inv()