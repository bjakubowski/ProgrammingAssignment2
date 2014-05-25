## This r script includes two functions. The first function is makeCacheMatrix.
## The second function is cacheSolve. Both are described in greater detail below,
## but calling cacheSolve(makeCacheMatrix(a)) determines if the inverse of an invertible
## matrix is saved in the cache. If so, it is retrieved from the cache, not
## calculated a second time. If not, it sets the inverse to the cache to reduce
## computation time if this inverse is later needed.

## makeCacheMatrix has three purposes. The first is to create a cached matrix m.
## The second is to set four subfunctions "set", get", "savecache", and "getcache".
## The argument for makeCacheMatrix is a matrix (invertible or otherwise).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   ### sets m to NULL
        set <- function(y) {
                x<<-y ### resets x to an input object y.
                m<<-NULL ### maintains m as NULL
        }
        get <- function() x ###prints the matrix "x"
        savecache <-function(inverse) m <<-inverse ### sets the cache "m" to a specified inverse,
        getcache <- function() m ### prints the cached matrix m
        list(set = set, get = get,
             savecache = savecache,
             getcache = getcache)
}


## cacheSolve produces the inverse of the matrix input to makeCacheMatrix.
## The arugment for cacheSolve is makeCacheMatrix(a), where a is the matrix that
## is being inverted. CacheSolve first determines if the inverse of matrix a is
## already available in the cache "m" [which is true if cacheSolve has already been
## called on makeCacheMatrix(a)].

## If so cacheSolve returns the inverse "m" after printing the message "getting
## cached data." If the inverse "m" has not already been computed, then
## cacheSolve calls the matrix "a" originally input into "makeCacheMatrix(a)",
## using the get() subfunction, then determines the inverse of a. Next it
## sets the cache "m" to this inverse using the savecache() subfunction,
## before printing m.

cacheSolve <- function(x, ...) {
        m <- x$getcache() ### produces m, the cached matrix from x 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$savecache(m)
        m ## Return a matrix that is the inverse of 'x' after saving to the cache.
}
