## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to set the value of the matrix, get the value of the matrix,set the value of the inverse matrix and get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinv <- function() m <<- solve(x)
        getinv<-function() m
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve computes the inverse, if not already exiting in the cache,  of the matrix returned by makeCacheMatrix function. If the inverse has already been calculated then the cachesolve retrieves it from the cache.
cacheSolve <- function(x, ...) {
        m<-x$getinv
        if (!is.null(m)) {
                message("inverse matrix already exists in teh cache")
                return(m)
        }
        data<-x$get
        m <- solve(data)
        x$setinv(m)
        m
}
