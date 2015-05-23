## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function can create a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## invM save the inverse
invM <- NULL

##Invalidate the cache
set <- function(y){
         x <<- y
         invM <<- NULL 
}

##Return the raw matrix
get <- function() x;

 
setinverse <- function(inv) inverse <<-inv;

##Get the cached inverse matrix
getinverse <- function() inverse;


return(list(set = set,get = get, setinverse = setinverse, getinverse = getinverse))
}


## Write a short comment describing this function
##This function computes the inverse of the special matrix which is returned from makeCacheMatrix.
##If the inverse has been calculated and the matrix has not changed, then this function should get inverse from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

inv <- x$getinv()

##Return the inverse if it has been cached
if(!is.null(inv)){
message("Getting the cached inverse")
return(inv)
}

##Otherwise, calculate and cache the inverse 
m <- x$get()
inverse <- solve(m,...)
x$setinverse(inverse)

return(inverse)

}
