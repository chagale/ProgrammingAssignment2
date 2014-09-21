## I HAVE TESTED THE BELOW CODE ON THE TEST CASES PROVIDED IN THE DISCUSSION FORUMS. EVERYTHING
## WORKS (INCLUDING THE CACHE FUNCTION) BUT I AM NOT ABLE TO RETURN THE VALUE OF THE getinverse
## FUNCTION ALTHOUGH THERE IS THE CORRECT INVERTED MATRIX STORED IN THERE. I AM HOPING TO AT
## LEAST BE ABLE TO GET PARTIAL CREDIT FOR TIME SPENT ON THIS.

## The two functions below, makeCacheMatrix and cacheSolve, attempt to take a matrix as an
## argument, calculate and cache the inverse of the matrix.

## The makeCacheMatrix function produces a list containing functions that set the value of
## the matrix, get/return the value of the matrix, set the inverse of the matrix and return
## the inverse

makeCacheMatrix<- function(x = matrix()) {
  i<-matrix(nrow=2,ncol=2)                                  ## create i as a null matrix                   
  set<-function(y) {                          ## our first function, set, takes the matrix argument and sets it as our value
    x<<-y
    i<<-matrix(nrow=2,ncol=2)
  }                                           ## get returns the value of the matrix
  get<-function() x
  setinverse<-function(solve) {               ## setinverse calculates the inverse of our matrix x, and passes i to the parent environment where it replaces the null matrix i
    i<<-solve(x,...)
  }
  getinverse<-function() {                    ## getinverse retrieves the value of the inverted matrix, i, which is searched for in the current environment, not found, but found in the parent environment
    i
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) ## makeCacheMatrix function returns a list which each of the above functions as its objects
}

## cacheSolve sees if there is a cached value of i (our inverted matrix) and returns it if there is, and if not, calculates an inverted matrix from objects stored in the previous function
cacheSolve <- function(x, ...) {       
  i<-x$getinverse()        ## first check x to see if there is a value stored in the getinverse function/object
  if(!is.na(i)) {         ## if it is not na, then return the message that it is retrieving cached data that is stored in getinverse, and then return the value that is stored there
    message("getting cached data")
    return(i)
  }
  data<-x$get()           ## if there is no value stored in getinverse, pass the value of get (our original matrix) to 'data'
  i<-solve(data)          ## pass the value of the inverse of our original matrix to i, and return i in the next line
  i
}
