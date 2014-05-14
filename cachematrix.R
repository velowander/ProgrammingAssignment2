#makeCacheMatrix stores the potentially computationally intensive matrix inversion
#It behaves like a "class" (see the function itself for more information)
#Note to self: later I will make this makeCacheMatrix function generic (rename function itself to makeCache,
# rename getInv() to getCachedResult() etc.)

# cacheSolve() is a "wrapper" around R's solve() function that using the caching functionality of makeCacheMatrix.
# Explanation of use:
# 1) Define the matrix you would like to invert
# 2) Call makeCacheMatrix() passing the matrix from step 1 as the only parameter. Store the result in a variable.
# 3) Pass the variable from step 2 as a parameter to cacheSolve(). The result from cacheSolve() is the answer.
#
# Example:
# >a <- makeCacheMatrix(matrix(c(8,-3,7,2),nrow=2,ncol=2)) #combines step 1 & 2
# >solve(a) #note result from standard R function
# >cacheSolve(a) #result from cached function should be the same, note on subsequent executions you get cache hit message

makeCacheMatrix <- function(x = matrix()) {
  #This is an implementation of "classes" in a functional language
  #This makeCacheMatrix() method is similar to a class factory, which in a language like Java would be a static method of a class.
  #It returns a list of functions (like Python, functions are passable first class objects and can be used as parameters)
  #These returned functions are the callable "methods" of the instantiated "class". Since they have names, they are callable much like instance methods.
  #See https://class.coursera.org/rprog-003/forum/thread?thread_id=87 for explanation by Community TA Richard Ambler
  inv <- NULL #we store the cached matrix inverse here, NULL means no cached value available yet
  #the set() and get() "methods" refer to the Matrix we wish to invert, whereas setInv() and getInv() refer to the inverse of said matrix
  set <- function(y) { #set the matrix for which we will cache the inversion
    x <<- y
    inv <<- NULL #If we set a new matrix, we want to eliminate the cached inverse of the previous matrix
  }
  get <- function() x #return the current matrix
  setInv <- function(inverseMatrix) inv <<- inverseMatrix
  getInv <- function() inv
  #This "class factory" is returning the following list, the list is the public "interface" of the class "instance"
  #The 4 functions listed below inside the returned list are the 4 callable instance methods
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Also see comments at top of file
  inv <- x$getInv()
  if(!is.null(inv)) { #cache hit!
    message("getting cached data")
    return(inv) #function terminated
  }
  matrix <- x$get() #if we've made it this far in the program, it is because of a cache miss
  inv <- solve(matrix, ...) #forum posters pointed out that ginv() is the matrix inverse function
  #the assignment instructions said solve() and solve() seems to give the same results, so I'll stick with it
  x$setInv(inv) #since to get here the cache was empty, we now cache the result
  inv
}
