##makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##makeCacheMatrix consists of  4 functions: set, get, setinv, getinv.
##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse, and setinv stores it in the object m in makeCacheMatrix.
##The functions depict the ability to assign values to variables in other environments that is not the current function execution environment of the function executed or the environment of the function definition.
 
##Input to makeCacheMatrix function is a matrix (we assume here matrix is invertible)
##makeCacheMatrix keeps a record of variable m.Hence initially set m as Null or empty vector. 
##set is a function that changes the vector stored in the main function.
##x<<-y assigns a new vector passed as argument to x in the enclosure of the inner functions so that the vector is available for all other inner functions to be accessed. 
##m is assigned null so that every time you make new vector because you do not want to take the value cached from a previous vector mean.
##get is a function that returns x stored in the main function.
##setinv simply stores the value of the input z in a variable m.assigning the inverse in a deeper environment to be cached and available for other functions such as getinv().
##getinv returns the inverse value on every call.
##Returns a list of 4 functions into the global environment.
 
makeCacheMatrix<- function(x=matrix()) { 
  m <- NULL  
  set <- function(y) { 
    x <<- y  
    m <<- NULL 
  }
  get <- function() x 
  setinv <- function(z) m <<- z 
  getinv <- function() m     
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)   
}

##call getinv() to return its handiwork and then evaporate when its work is done.On the first call of var$getinv(), getinv finds m stored in makeCacheMatrix's environment and finds it is empty.
##The next time we call var$getinv() m is no longer NULL so it simply returns m, saving us a lot of cpu resources.
##in the first call m is null hence the if is not executed,and the below statements are executed.
## the get() is called and the input is retrieved.
##the inverse of the matrix is computed.
##the setinv() function is called to store of the value of the matrix inverse.
##returns the matrix inverse.


cacheSolve<- function(x, ...) { 
  m <- x$getinv()  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }  
  data <- x$get() 
  m <- solve(data, ...) 
  x$setinv(m) 
  m    
}
