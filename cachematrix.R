# Matrix inversion is usually a costly computation and their may be some 
# benefit to caching the inverse of a matrix rather than compute it 
# repeatedly. These two functions are used to cache the inversion of a matrix.


# makeCacheMatrix: This function returns a list of functions to:
#1. set a matrix
#2. get a matrix
#3. set the inverse of matrix in cache
#4. get the inverse matrix from cache


makeCacheMatrix <- function(matrix=matrix()){
    invMatrix <-NULL
    setMatrix <- function(x){
        matrix<<-x
        invMatrix<<-NULL
    }
    getMatrix<- function() matrix
    setInvMatrix<-function(x) invMatrix<<-x
    getInvMatrix<-function() invMatrix
    list(setMatrix=setMatrix,getMatrix=getMatrix,setInvMatrix=setInvMatrix,
         getInvMatrix=getInvMatrix)
}
 
# cacheSove: This function computes the inverse of the matrix stored in makeCacheMarix list. It first checks to see if 
# the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, 
# it computes the inverse by solve() function and sotres the computed inverse in the cache of makeCacheMatrix list. 
# 
#  This function assume that the matrix supplied is always invertible.

cacheSolve <- function(cacheMatrix, ...){
    invMatrix <- cacheMatrix$getInvMatrix()
    if(!is.null(invMatrix)){
        message("getting cached inverse")
        return(invMatrix)
    }
    matrix<-cacheMatrix$getMatrix()
    invMatrix <- solve(matrix, ...)
    cacheMatrix$setInvMatrix(invMatrix)
    invMatrix
}

## Sample run
## 
## 
## > mat<-matrix(1:4,2,2)
## > cachedMatrix<-makeCacheMatrix(mat)
##
## Computes the inverse in first run
## > cacheSolve(cachedMatrix)
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## 
## Subsequent run returns result from the cache
## > cacheSolve(cachedMatrix)
## getting cached inverse
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## 



