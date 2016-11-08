## makeCacheMatrix creates list to be used in cacheSolve function
## makeCacheMatrix sets and gets the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set = function(y){
		x<<-y
		inv <<-NULL
	}
	get = function() x
	setinv = function(inverse) inv <<- inverse
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## inverse of makeCacheMatrix return

cacheSolve <- function(x, ...) {
        
        ## if the inverse has already been calculated get it from the cache and skips computation
        
        inv = x$getinv()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        
        ## if not, calculate inverse
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
