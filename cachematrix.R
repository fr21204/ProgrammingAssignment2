## Put comments here that give an overall description of what your
## functions do
# Creation of the inverse of a matrix and put it on a cache.

## Write a short comment describing this function
#The function makeCacheMatrix computes the inverse of the matrix
makeCacheMatrix <-function (x = matrix()){
        # Initialization of the 2 objects x and m
        m<- NULL
        set <- function (y) {
                x <<- y
                # y is object of the parent environment x
                m = NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function () m
        list (set = set,  # gives the name 'set' to the set() function defined above
              get= get, # gives the name 'get' to the get() function defined above
              setinv = setinv,  # gives the name 'setinv' to the setinv() function defined above
              getinv = getinv)  # gives the name 'getinv' to the getinv() function defined above
}


## Write a short comment describing this function
# ## Return a matrix that is the inverse of 'x' if is already calculated in the previous function and if not calculates the inverse matrix

cacheSolve <- function (x, ...){ 
                m <- x$getinv()#call the getinv() function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #execute function solve() if m is NULL
        x$setinv(m)
        m
}