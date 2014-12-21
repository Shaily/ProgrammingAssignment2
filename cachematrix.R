##This function acts a constructor for matrix and defines all getter setter methods
##for a matrix. It finally creates a list of functions : getter and setter methods for
##both the inverse of the matrix and the matrix itself. <<- assignment operator is used
##to make these internal variables private to this method
makeCacheMatrix <- function(x = matrix()) {
        # declaring and defining a variable for inverse of the matrix x
        inv <- NULL
        
        # A setter function for matrix x
        set <- function(y){
            x <<- y 
            inv <<- NULL
        }
        
        # A getter function for matrix x
        get <- function() x
        
        # A setter function for the inverse of the matrix x : inv
        setInv <- function(inverse){
          inv <<- inverse
        }
        
        # A getter function for the inverse of the matrix x : inv
        getInv <- function() inv
        
        # creating a list of these funtions to be used later
        list(set=set , get=get, setInv=setInv, getInv=getInv)
}

##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
 
        ##getting the inversed of the matrix x
        inv <- x$getInv()
        
        ##if the inverse of the matrix is not calculated before, it will be null
        if(!is.null(inv))
        {
          message("Getting the cached value")
          return (inv)
        }
        
        ##if the inverse is not calculated yet, we get the matrix x and 
        ##calculate its inverse and store it in inv
        data <- x$get()
        inv <- solve(data)
        
        #using the setter method of x, set the inverse of the matrix x
        x$setInv(inv)
        
        #return the inv
        inv
}

#Example to use 
c=rbind(c(1, -1/4), c(-1/4, 1))
x <- makeCacheMatrix(c)
xInv <- cacheSolve(c)
