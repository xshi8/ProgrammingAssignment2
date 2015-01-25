## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix () creates a list of set,get,setinv,getinv functions.
## these function can be called individually by subsetting makeCacheMatrix()
## for example, makeCacheMatrix(m)$get()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                 # assigns NULL to inv (a placeholder for inversed matrix)
  
    set <- function(y) {
               x <<- y                          # assigns new matrix value y to input matrix x 
               inv <<- NULL                     # assigns NULL to inv 
    }
  
    get <- function() x                         # returns the setted matrix x
  
    setInv <- function(inverse) inv <<- inverse # assigns the inversed matrix to inv
    getInv <- function() inv                    # returns the setted inversed matrix
  
    # returns a list that contains set(), get(),setInv() and getInv functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## cacahSolve() uses makeCacheMatrix(m) as argumnet x  
## to call different functions depending on the inversed 
## matrix is cached or not

cacheSolve <- function(x, ...) {
    minv <- x$getInv()       # get the inversed matrix from x
    if(!is.null(minv)) {     # if the inversion matrix is cached
         message("getting cached data")
         return(minv)        # return the cached inversed matrix
    }
    m <- x$get()             # if not cached, then gets the matrix from x
    minv<- solve(m)          # Returns a matrix that is the inverse of m
    x$setInv(minv)           # calls setinv()
    x$getInv()               # calls getinv()
        
}
