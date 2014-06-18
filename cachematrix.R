## These functions allow for the storing of a matrix, solving and storing of the matrix's inverse, and the calling of the matrix and its inverse

## This creates a function that inputs a list of functions into a variable that can be called as a subset to set/store the matrix, call the matrix, set the inverse matrix but not create it, and call the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL  ## Empties s
    set <- function(y) {  ## Creates the set function to set the matrix and empty s
        x <<- y
        s <<- NULL
    }
    get <- function() x ## Creates the get function to print the set matrix
    setsolve <- function(solve) s <<- solve ## Most important for caching in the next function, makes s equal to solve, which will be the inverse matrix in the cacheSolve funtion
    getsolve <- function() s ## Creates the function to retrieve the inverse matrix after it is created
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve) ## Creates a list of functions to be called as subsetted functions
}

## This function creates the inverse matrix and stores to be called later so it does not need to be calculated later
cacheSolve <- function(x, ...) {
    s <- x$getsolve() ## Sets s equal to the cached solve
    if(!is.null(s)) {  ## Checks if the cached solve is null or actual data. If it is actual data then it prints the message below and s
        message("getting cached data")
        return(s)
    }
    data <- x$get() ## Gets the matrix inputted using the previous function
    s <- solve(data, ...) ## Creates the inverse matrix sets it equal to s
    x$setsolve(s) ## Stores the inverse matrix in the function
    s ## Displays the inverse of the special matrix
}