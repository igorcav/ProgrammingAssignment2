## Funções para calcular matriz inversa e retornar, caso já tenha sido calculada, a que estiver em cache

## Cria uma matriz que armazena em cache a sua inversa

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Procura se existe a inversa da matriz em cache e, caso não haja, então calcula e armazena

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if( !is.null(m) ) return(m)
        m <- solve(x$get()) ##Calcula o inverso da matriz
        x$setinverse(m)
        m
}

