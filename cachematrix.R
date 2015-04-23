## USAGE
## From your working directory...
## source("cachematrix.R")    load R program
## a <- makeCacheMatrix()     create functions
## a$set(matrix(1:4, 2, 2))   create matrix
## cacheSolve(a)              initial call returns inverted matrix and caches the matrix
## cacheSolve(a)              subsequent calls returns inverted matrix from cache


##makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	#--cache is used to store the cached value. initialize to NULL
        cache <- NULL

    #--create the matrix in the working environment "<<-"
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }

    #--get the value of the matrix
       get <- function() x
    #--invert the matrix and cache the matrix
        setMatrix <- function(inverse) cache <<- inverse
    #--get the inverted matrix from cache
        getInverse <- function() cache

    #--return the created functions to the working environment
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)


}


##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    #--get the inverse of the cached matrix
        cache <- x$getInverse()
	#--if inverted matrix exists, return it exists
        if (!is.null(cache)) {
             #--return matrix to the console
             	message("matrix is cached")
                return(cache)
        }
	#--matrix was not cached, create new matrix
       matrix <- x$get()
    #--trying try/catch    
        tryCatch( {
         #--set then return inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
            #--cache inverted matrix
                x$setMatrix(cache)
        } )

     #--return matrix to console
        return (cache)
        
}
