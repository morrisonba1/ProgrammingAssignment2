## Functions that cache the inverse of a matrix
## First function makeCacheMatrix. Second function cacheSolve

## makeCacheMatrix creates a special matrix, which creates s list
## containing a function to;
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
if (ncol(x)==nrow(x) && det(x)!=0){
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse <- function(inverse) m<<- inverse
	getinverse<-function() m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}else{
	return(message("The matrix is not invertible"))
}
}

## cacheSolve caculates the inverse of the special matrix created by
## makeCacheMatrix. The steps in the processing are;
## check to see if the inverse matrix has already been caculated
## if so gets the inverse matrix from the cache and skips the computation.
## otherwise it calculates the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<- solve(data, ...)
	x$setinverse(m)
	m
}

