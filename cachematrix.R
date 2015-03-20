## You can imagine this whole work as a watch. First function creates the buttons of this watch, that we can
## use them for calling what we want later. In this case inversing a matrix. One button for setting the matrix
## to cache, the other one for calling it later, the other one is caching inverse matrix and the last one calling 
## inverse matrix at the end.


## We stored the values in nested functions here. We will need these values later to calculating
## inverse matrix. Then we made a list of these nested functions. This list is the button parts of the watch that
## I've mentioned above

makeCacheMatrix <- function(m = matrix()) {
        m_inv<-NULL
        set<- function (n) {
                m <<- n 
                m_inv <<- NULL
        }
        get <- function() m
        set_inv<- function(inverse) m_inv <<- inverse
        get_inv<- function() m_inv 
        list( set = set, get = get, set_inv = set_inv, get_inv = get_inv)
        
}


## At first we are checking is there any inverse matrix in cache. If there is, we just returning it as an answer
## If there is not, we are calling the matrix from cache and calculating inverse matrix. Before printing the 
## answer, we are caching inverse matrix for using it next time. so, if the same matrix will be used again,
## re-calculating will be avoided and inverse will be called from cache.

cacheSolve <- function(k, ...) {
        m_inv<- k$get_inv()
        if (!is.null(m_inv)) {
                message ("getting cache data")
                return(k$get_inv())
        }
        data<-k$get()
        m_inv<- solve(data, ...)
        k$set_inv(m_inv)
        m_inv
        
}