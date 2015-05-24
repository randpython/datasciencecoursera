#cacheSolve fucntion takes makeCacheMatrix object as input
#Checks if the inversion of matrix already calculated an if yes
#then calls the saved object and saves time 

cacheSolve <- function(x, ...) {
        
        #l_inversed_matrix is the local matrix object
        #Call the getinverse function from makeCacheMatrix object which 
        #was already created and passed as format argument to this function 
        
        l_inversed_matrix = x$getinverse()
        
        #Check the returned value from getinverse function and check its return
        #value; if its not null then e function ends and returns the 
        #inverted matrix value which was returned from getinverse function above
        
        if (!is.null(l_inversed_matrix)) {
                message("Getting the cached value")
                return(l_inversed_matrix)
        }
        
        #If the code reaches this point means the inverse of the matrix needs
        #be calculated and stored in makeCacheMatrix object
        
        l_matrix <- x$get()
        l_inversed_matrix <- solve(l_matrix,...)
        x$setinverse(l_inversed_matrix)
        l_inversed_matrix
}