
## the 2 functions below are meant to calculate and cahce the inverse of a matrix


# makeCacheMatrix is creating a list of functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv<- NULL
        
        set<- function(y){
                x<<-y
                inv<<-NULL}
        get<- function() x
        setinverse<- function(inverse)inv<<-inverse
        getinverse<-function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
        
}

# This function returns the inverse of the matrix. It first checks if
# the inverse has already calculated.if it did, it gets the result from cahche.
# If not, it calculate the inverse, and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)}
        data<- x$get()
        inv<- solve(data)
        x$setinverse(inv)
        inv
}
