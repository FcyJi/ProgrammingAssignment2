
# The 1st function create a special object which is a matrix that 
# can cache its inverse.
# The second one  generate the inverse if it has not been yet.


# This function permits to create a matrix that can cache its own inverse.
makeCacheMatrix <- function(x = matrix()) {
  s<- NULL
  
  # Set the value of the matrix
  set<- function(y){
    x<<-y
    s<<- NULL
  }
  
  get<- function() x                         # Get the value of the matrix.
  setInv <- function(solve) s <<- solve      # Set the value of the inverse.
  getInv<- function() s                      # Get the value of the inverse.
  
  # List of elements to be returned.
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}



# This function permits to generate the  inverse of the 
# special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInv()
        
        # Checked if the inverse has been calculated.
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        m <- solve(data, ...)    # Calculate the inverse.
        x$setInv(s)
        s                        # Return the inverse of the matrix.
}
