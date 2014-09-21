## The two functions below, makeCacheMatrix and cacheSolve, attempt to take a matrix as an
## argument, calculate and cache the inverse of the matrix.

## The makeCacheMatrix function produces a list containing functions that set the value of
## the matrix, get/return the value of the matrix, set the inverse of the matrix and return
## the inverse

  makeCacheMatrix<- function(x = matrix()) {
        i<-matrix(nrow=2,ncol=2)                    ## create i as a null matrix                                                             ## get returns the value of the matrix
        set<-function(y) {                          ## set takes the matrix argument and sets it as our value
          x<<-y
          i<<-matrix(nrow=2,ncol=2)
         }                                          ## get returns our value (matrix)
        get<-function() x
        getinverse<-function() solve(x)             ## getinverse returns the inverse of our matrix value
        list(set=set,get=get,getinverse=getinverse) ## makeCacheMatrix function returns a list which each of the above functions as its objects
      }

## cacheSolve sees if there is a cached value of i (our inverted matrix) and returns it if there is, and if not, calculates an inverted matrix from objects stored in the previous function
  cacheSolve <- function(x, ...) {       
        i<-x$getinverse()                           ## first check x to see if there is a value stored in the getinverse function/object
        if(!is.null(i)) {                             ## if it is not null, then return the message that it is retrieving cached data that is stored in getinverse, and then return the value that is stored there
          message("getting cached data")
          return(i)
        }
        data<-x$get()                               ## if there is no value stored in getinverse, pass the value of get (our original matrix) to 'data'
        i<-solve(data)                              ## pass the value of the inverse of our original matrix to i, and return i in the next line
        i
      }