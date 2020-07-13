
makeCacheMatrix <- function(x = matrix()) {
        ##this function returns a list of four functions, which
                ##1. set the value of a matrix 'x'
                ##2. get the value of a matrix
                ##3. set the value of the inverse matrix
                ##4. get the value of the inverse matrix
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) i <<-inverse
        getinverse <- function ()i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
        ## This function returns the inverse matrix of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        } #if inverse matrix has been saved in cache it is returned with message
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i) ##if not, inverse is calculated and stored in cache
        i
}
