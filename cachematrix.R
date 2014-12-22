#
# The two functions in this source file illustrate a pattern for caching the 
#   result of a potentially long-running computation.  In this case, the computation is 
#   inverting a matrix.
# The overall approach is to
#   - Call a set-up function that creates a closure for the cached value and its key.
#     It will expose methods to assign the key and save and retreive the cached value.
#   - Call a function to perform the desired computation (matrix invere); it will use 
#     the first function to retrieve the result, if cached, or comptue and save it, 
#     before returning it.

# illustration of use:
# mx <- c(8, 1, 6, 3, 5, 7, 4, 9, 2)
# dim(mx) = c(3,3)
# cmx <- makeCacheMatrix(mx)
# cacheSolve(cmx)         # returns inverse with cache miss message
# cacheSolve(cmx)         # returns inverse with cache hit message


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

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    # method sets the matrix value and resets the inverse to NULL (not calculated)
    set <-  function(y) {
        mat <<- y
        inv <<- NULL
    }
    # method to return the matrix
    get <- function() mat
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

cacheSolve <- function(mkfn, ...) {
    # get the inverse from cache, possibly null
    inv <- mkfn$getinv()
    if (!is.null(inv)) {
        # result already cached, will return inv
        message("found cached inverse")
    } else {
        # result not cached, must compute
        message("inverse not cached--calculating")
        mdta <- mkfn$get()
        # throw an error if matrix cannot be inverted
        if (nrow(mdta) != ncol(mdta)) {
            stop("Attempt to invert a non-square matrix")   
        } else if (det(mdta) == 0) {
            stop("Attempt to invert a singular matix")
        } else {
            inv <- solve(mdta, ...)          # will return inv...
            mkfn$setinv(inv)                 # and cache for future reference
        }
    }    
    inv  
}
