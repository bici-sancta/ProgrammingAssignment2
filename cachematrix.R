#
#...	Johns Hopkins Universtity - MOOC
#...		R Programming Language, Data Scientist's Toolkit Series
#...			Programming Assignment 2
#...				Jan-2015
#
#...	Assignment is to use lexical scoping / cache capability of R
#...		to return matrix inverse &/or cached inverse
#
#...	Usage example :
#			$ A <- matrix(c(1,0,1,1,2,1,1,0,4), nrow=3, ncol=3)
#			$ A_lst <- makeCacheMatrix(A)
#			$ A_inv <- cacheSolve(A_lst)  	# solves and returns A_inv
#			$ A_inv <- cacheSolve(A_lst)	# returns cached version
#
#		in either case, test can be accomplished:
#			$ Ident <- A %*% A_inv			# should return Identity matrix 
#
#	-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#		Return list of functions that provide lexical scoping
#	-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function()
  {
  	x
  }
  set_inv <- function(x_inv)
  {
  	inv <<- x_inv
  }
  get_inv <- function ()
  {
  	inv
  }

  list(set = set,
  		get = get,
  		set_inv = set_inv,
  		get_inv = get_inv)
}

#	-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#		Matrix Inversion or return cached value
#	-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'

  inv <- x$get_inv()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve (data, ...)
  x$set_inv(inv)
  inv
}

#	-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#		end of file
#	-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
