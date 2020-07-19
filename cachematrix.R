## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a list containing 4 functions set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<- function (y){
        x<<-y
        i<<-NULL
    }
    get<-function(){x}
    setInverse<-function(inverse){i<<-inverse}
    getInverse<-function(){i}
    list( set= set, get=get, setInverse=setInverse, getInverse =getInverse)
}


## Write a short comment describing this function
## The cacheSolve function saves the inverse of the matrix in i
## (if it was calculated before, it gets it from the cache 
##if it does not calculate it and saves it with setInverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getInverse()
    if(!is.null(i)){
        message(("getting cached data"))
        return(i)
    }
    mat<-x$get()
    i<-solve(mat,...)
    x&setInverse(i)
    i
}
