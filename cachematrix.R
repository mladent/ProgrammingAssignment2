##*************************************************************##
##   Functions enable cached processing of matrix inversion    ##
##*************************************************************##

# 1) call makeCacheMatrix function to create a special matrix
#    to be used in caching
# 2) call cacheSolve function to invert the matrix
# 3) special extension of functions to recognise arguments used


## e.g. convert mymatrix to special matrix by
# > myspecial_matrix <- makeCacheMatrix(mymatrix)

## First call to 'cacheSolve' will solve the matrix 
# > myspecial_matrix_inverted <- cacheSolve(myspecial_matrix)

## Second call to 'cacheSolve' will only collect the catched result 
# > myspecial_matrix_inverted <- cacheSolve(myspecial_matrix)

## Change myspecial_matrix to my_new_matrix
# > myspecial.matrix$set(my_new_matrix)



##*************************************************************##

## makeCacheMatrix
#     Used to create a special matrix to be used for
#     cached processing

makeCacheMatrix <- function(x = matrix()) {

      # reset inverted matrix
      inv <- NULL
            
      ## making possible to Set/Reset the Matrix to y
      set <- function(y) {
            # assign to global 'x' the element 'y' used in set
            x <<- y
            
            # reset temp. inverted matrix
            inv <<- NULL
      }
      
      ## GET to retrieve the matrix stored in 'x'
      get <- function() x
      
      ## set the inverse value of a 'special' matrix to global inv
      setinverse <- function(inverse) inv <<- inverse
      
      ## retrieve the inverse value of a 'special' matrix from inv
      getinverse <- function() inv
      
      
      
      ## SPECIAL CHECKING FOR MATRIX AGRGUMENTS 
      
      # reset arguments
      argm <- NULL
      # set arguments of matrix
      setarguments <- function(args) argm <<- args
      # retrieve the arguments
      getarguments <- function() argm
      
      ## return 'results' as a list
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse,
           setarguments = setarguments,
           getarguments = getarguments)
      
}


## cacheSolve
#    Used to solve the special matrix
#     If called for the first time it computes the inverse
#     If called second time with the same matrix returns the cached value

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      # get the inverted value
      inv <- x$getinverse()
      
      # collect the arguments used in calling the function
      argusNew <- list(...)
      argusOld <- x$getarguments()
      
      ## IF all the matrix arguments are identical and
      #     there is a value i.e. not NULL 
      if( identical(argusOld, argusNew) && !is.null(inv)) {
            message("getting cached inversed matrix")
            
            # return the inverted value
            return(inv)
      }
      else {
            # if there is no value i.e. NULL
            #   or if the arguments are different:
            # calcualate the new result
            
            
            # get the matrix
            matriX <- x$get()
            
            # calculate the inverted matrix
            inv <- solve(matriX, ...)
            
            # set the new results and also new arguments
            x$setinverse(inv)
            x$setarguments(argusNew)
            
            
            # return the inverted matrix results
            return(inv)
      }

}


##*************************************************************##

