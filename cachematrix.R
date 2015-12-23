# makeCacheMatrix:  function to ingest the matrix with its variables and calculate the matrix's inverse
#	sample input to makeCacheMatrix:  
#		b <- matrix(1:4,2,2)
#		a <- makeCacheMatrix(b)
#	assumes only invertible matrix will be used

# m = indicates the existence of the inverse matrix
# x = the originating matrix
# y = the matrix value that available in environments other than its current environment


makeCacheMatrix <- function(x = matrix()){
    m <- NULL    				          #establishes the current condition
    set <- function(y) {			    #set function modifies the vector stored in an existing makeCacheMatrix object
      x <<- y
      m <<- NULL
    }
    get <- function(){			      #get function used to access the value give to makeCacheMatrix, e.g. access a in makeCacheMatrix(a)
      x
    }
    setinv <- function(solve) {		#setinv function calcuates the inverse of the matrix
      m <<- solve
    }
    getinv <- function() {			  #getinv function retrieves the calculation of the inverse of the matrix
      m
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)	#returned list of four functions
}
  


# cacheSolve:  function to which the matrix's inverse can be retrieved if a cached version exists, if not, then it calculates the matrix's inverse
#	sample input to cacheSolve:  
#		cacheSolve(a)

cacheSolve <- function(x, ...)  {
  m <- x$getinv()				    #retrieves x's getinv() from makeCacheMatrix
  if(!is.null(m)) {			  	#looks to see if the value retrieved is not null
    message("getting cached data")#if it isn't nulll, display message indicating that the retrieved cache will be used
    return(m)				        #display the cached value that was retrieved
  }
  data <- x$get()				    #assign the get function to data
  m <- solve(data, ...)			
  x$setinv(m)					      #calcuate the inverse of the matrix
  m						              #display the value
}

