## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

cacheSolve <- function(chacheContainer, ...) {
        ## Return a matrix that is the inverse of 'x'
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
M=rbind(c(1, -1/4), c(-1/4, 1)) 
cm<-makeCacheMatrix(M)
print(cm$get())
cacheSolve(x=cm)
