## La función makeCacheMatrix() toma como parámetro una matrix, la cual genera 
## 4 funciones internas: set, get, setinverse y getinverse y se ejecutan dentro
## del ambiente de la función, dando como resultado la inversa de la matrix.

## Funcion que genera la inversa de la matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function() inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Función que toma la inversa del cache de la función anterior.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
