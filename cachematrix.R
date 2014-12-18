## Put comments here that give an overall description of what your
## functions do

## create a "special matrix" to maintain a cached value: this is essentially
#      an object with methods and encapsulated data as elements of a list
#    - the use case is caching a matrix inverse but this construct is actually 
#      flexible and general purpose
#    - encapsulated data members:
#       x = the cached value of the matrix
#       i = the cached inverse of the matrix (initially  NULL)
#    - methods (getters/setters)
#       set = function to accept and save the matrix, and reset the inverse (to NULL)
#       get = function to return the cached matrix
#       setinv = function to cache the value of the inverse of the matrix
#       getinv = function to return the cached value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # method sets the matrix value and resets the inverse to NULL (not calculated)
    set <-  function(y) {
        x <<- y
        i <<- NULL
    }
    # method to return the matrix
    get <- function() x
    # method to cache the value of the inverse of x (calculated externally)
    setinv <- function(i) inv <<- i
    # method to get the cached inverse (may be NULL if invalid)
    getinv <- function() inv
    # returns the list which works like an object with getters and setters
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## this function uses the object-like list returned by makeCacheMatrix to compute
#    the inverse of a matrix
#    - the value of the inverse is simply returned from cache if it has previously
#      been set
#    - if cached inverse was not set, it is computed (using solve()) and stored
#      (using the setinv() method of makeCacheMatrix)
#    - (not shown here) the value of the matix can be changed by passing it to the
#      set() method of the list-object
#  

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        message("found cached inverse")   # will return i
    } else {
        message("inverse not cached--calculating")
        data <- x$get()
        # throw an error if matrix cannot be inverted
        if (nrow(data) != ncol(data)) {
            stop("Attempt to invert a non-square matrix")   
        } else if (det(data) == 0) {
            stop("Attempt to invert a singular matix")
        } else {
            i <- solve(data, ...)          # will return i...
            x$setinv(i)                    # and cache for future reference
        }
    }    
    i  
}
