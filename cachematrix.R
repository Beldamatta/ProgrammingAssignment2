## The following two functions cache the inverse of a matrix 


## makeCacheMatrix() function creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set<- function(mtx){ # sets the value of x
          # the <<- assignment operator will assign the value
          # on the right side of the operator to an object in
          # the **parent environment** named by the object on
          #the left side of the operator
            x <<- mtx
            i <<- NULL # this line means that every time
            # x is reset, i cached in the memory
            #is cleared
        }
        get <- function() x # get the value of x
        setinverse <- function(inverse) i <<- inverse #defines 
        #setter for the inverse matrix i
        getinverse <- function() i # defines getter for
        # the inverse matrix i
        list(set=set, get=get, setinverse=setinverse, 
             getinverse=getinverse)
        # list() will assign each of the functions as an
        #element within a list, to be returned to the 
        #parent environment
}


## cacheSolve() is a function that computes the inverse
##of the special matrix returned by makeCacheMatrix. If 
##the inverse has already been calculated, then the function
##will return the cached inverse.

cacheSolve <- function(x,...) {
        i <-x$getinverse() # calls the getinverse() function
        #on the input object
        if(!is.null(i)){
          message("getting cached data")
          return(i) # if i already was calculated, its value
          #is returned from the cache, accompanied by the
          #above message
        }
        data <- x$get() # calls the get() function on the
        #input object
        i <- solve(data) #executes the function solve() on
        #on the input object
        x$setinverse(i) # setts the inverse
        i # returns the inverse matrix
}


