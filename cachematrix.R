## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){                                              ##fun to create a special "matrix"
        
        i <- NULL
        
        set <- function(y){                                                             ##fun to set matrix of special matrix object
                
                x <<- y
                
                i <<- NULL
        }
        
        get <- function() x                                                             ##fun to get stored matrix from special matrix object
        
        setinverse <- function(inverse) i <<- inverse                                   ##fun to assign inverse matrix to cache of special matrix object
        
        getinverse <- function() i                                                      ##fun to get cache inverse matrix from special matrix object
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

## Write a short comment describing this function

cacheSolve <- function(x, ...){                                                         ##fun to run solve on special matrix object and cach inverse to the special matrix
        
        i <- x$getinverse()                                                             ##extract cached inverse from special matrix x
        
        if(!is.null(i)){                                                                ##test if special matrix x has a cached inverse value, if yes return value and break
                
                message("getting cached data")
                
                return(i)
        }
        
        data <- x$get()                                                                 ##extract matrix from special matrix x
        
        i <- solve(data, ...)                                                           ##run solve fun on data, producing the inverse matrix
        
        x$setinverse(i)                                                                 ##put the calculated inverse matrix in the cache of special matrix object x
        
        i
        
}




