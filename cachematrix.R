## The makeCacheMatrix function creates a special "matrix", which is represented by 
## a list containing functions to 
##                      1. Set the value of the matrix
##                      2. Get the value of the matrix
##                      3. Set a value to the inverse matrix of the original matrix 
##                      4. Get the value of the inverse matrix

## The cacheSolve function computes the inverse of the matrix created with the makeCacheMatrix function.
## This  function first checks whether the inverse matrix has been already computed. If so, it gets
## the inverse matrix from the cache and skips any additional calculations. Otherwise, it computes
## the inverse of the original matrix and sets value of that matrix in the cache via new.inverse.matrix()

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(matrix = matrix()) {
        
        inverse.matrix <- matrix() ## creates an empty inverse matrix in the local environment
        
        set.new.matrix <- function(new.matrix){
                matrix <<- new.matrix
                inverse.matrix <<- matrix()
        } ## creates a new matrix for inversion; resets the cached inverse matrix to an empty one
        
        get.matrix <- function() matrix
        set.inverse.matrix <- function(new.inverse.matrix) inverse.matrix <<- new.inverse.matrix
        get.inverse.matrix <- function() inverse.matrix
        
        list(new.matrix = set.new.matrix,
             matrix = get.matrix,
             new.inverse.matrix = set.inverse.matrix,
             inverse.matrix = get.inverse.matrix)
}


## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function

cacheSolve <- function(matrix, ...) {
        
        inverse.matrix <- matrix$inverse.matrix() ## retrieves the inverse matrix
        
        if(all(is.na(inverse.matrix)) == FALSE){
                message("getting cached matrix")
                return(inverse.matrix)
        } ## checks whether the inverse matrix is not empty; if so returns the cached inverse matrix
        
        data.new.matrix <- matrix$matrix() ## retrieves the matrix to be inverted
        inverse.matrix <- solve(data.new.matrix) ## computes the inverse matrix
        matrix$new.inverse.matrix(inverse.matrix) ## caches the value of the inverse matrix
        
        inverse.matrix ## prints the inverse matrix to the console
}
