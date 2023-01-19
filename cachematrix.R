##I created two functions makeCacheMatrix and cacheSolve, which cooperate to cache and calculte 
##matrix inverse



##The function makeCacheMatrix returns a list with four functions: set, get, setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #At the start, inv is set to NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x #get the matrix x
  setinv <- function(inverse) inv<<-inverse #cache the inverse of the matrix
  getinv <- function() inv #get the inverse of matrix
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##The function cacheSolve checks if the inverse for the current matrix is cached, and if not, calculate it and cache it
##The function cacheSolve finally returns the inverse for the matrix
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){ #check if the inverse is cached
    message("Getting cached data!")
    return(inv)
  }
  matrixx <- x$get()
  inv <- solve(matrixx,...) #calculate the inverse if not cached
  x$setinv(inv) #cache the inverse
  inv #return the inverse
}
