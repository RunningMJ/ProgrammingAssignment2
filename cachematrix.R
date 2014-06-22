## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ##removes the class of unknown object 'm'
        set <- function(y) {
                x <<- y
                m <<- NULL
        } ##created vector "set" which contains the values input into makeCacheMatrix function
        getmatrix <- function() x ##returns the values of the vector x
        setinverse <- function(solve) m <<- solve #function to set the inverse of the classless 'm'
        getinverse <- function() m #returns the inverse of 'm' as a function
        list(set = set, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse) #creates a list of all the function created above
}


cacheSolve <- function(x, ...) {
        m <- x$getmatrix() #places previus matrix in 'm' variable
        if(!is.null(m)) {
                message("getting cached data")
                return(m) #tests whether m is the same matrix as already computed in makeCachematrix, returns if true
        }
        data <- x$getmatrix() #makes new matrix if different from previous
        m <- solve(data, ...) #computes the inverse of new matrix
        x$setinverse(m)
        m  ## Return a matrix that is the inverse of 'x'
}
