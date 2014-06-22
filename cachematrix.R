
## This function is used to encapsulate an object and a chached value
## associated with it.It returns 4 functions, to get and set values for
## these two objects. The functions get and set are used to access the 
## object and getChached and setChased are used to access the chached value.

makeCacheMatrix <- function(x = matrix()) {
   cache<-NULL
   set <- function(y){
      x <<- y
      cache<<- NULL
   }
   get <- function() x
   setCached <- function(value) cache <<-value
   getCached <- function() cache
   list(set = set,get = get,
        setCached = setCached,
        getCached = getCached)
}

## This function is used to compute and cache the inverse of a matrix. It 
## gets as input a custom object used for chaching a value, that has to contain
## at least list of 3 functions with names: getCached, setChached, get. If
## the input already contains the chached inverse of the matrix, then this value
## is returned by the function. Otherwise the inverse is computed and stored in
## the object.
cacheSolve <- function(chacheContainer, ...) {
  
   inv<-chacheContainer$getCached()
   if(!is.null(inv)){
      message("getting cached data")
      return(inv)
   }
   data <- chacheContainer$get()
   inv<-solve(data,...)
   chacheContainer$setCached(inv)
   inv   
}

#Test code here:
M=rbind(c(1, -1/4), c(-1/4, 1)) 
cm<-makeCacheMatrix(M)
print(cm$get())
print(cacheSolve(cm))
print(cm$getCached())
print(cacheSolve(cm))
