## Creates a "special matrix", which is basically a list of function, which enables to save the inverse of the matrix with the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                                 #initial state with no inverse calculated
        setmatrix <- function(y) {                      #function to set the actual matrix values
                x <<- y
                inverse <<- NULL
        }
        getmatrix <- function() x                       #function to get the saved matrix
        setinverse <- function(inv) inverse <<- inv     #function to set the inverse
        getinverse <- function() inverse                #function the get the inverse
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function to calculate the inverse of the matrix (if not done before) and "append" it to the "special matrix".

cacheSolve <- function(x, ...) {
                                                        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()                       ## gets inverse stored in "CacheMatrix" x
        if(!is.null(inverse)) {                         ## checks if there is already an calculated inverse. 
                message("getting cached inverse")       ## if there is, then then inverse get return after a confirming message
                return(inverse)
        }
        mat <- x$getmatrix()                            ## if no inverse is calculated, then...
        inverse <- solve(mat, ...)                      ## ...it is calculated here
        x$setinverse(inverse)                           ## and passed to the "CacheMatrix" object with the setinverse function
        inverse                                         ## returns calculated inverse
}