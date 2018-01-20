## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                i<-NULL
                set<-function(y){
                  x<<-y
                  i<<-NULL
                }
                get<-function()x
                setinverse<-function(inverse) i<<-inverse
                getinverse<-function()i
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Returning a matrix that is the inverse of 'x'
    i<-x$getinverse()
   if(!is.null(i)) {
          message("Getting cached data of matrix inverse")
          return(i)
   }
   mat <- x$get()
   i<- solve(mat, ...)
   x$setinverse(i)
   i
}

#testing for correctness
mymatrix<-makeCacheMatrix(matrix(1:4,2,2))
mymatrix$get()
mymatrix$getinverse()
cacheSolve(mymatrix)
cacheSolve(mymatrix)
#completed