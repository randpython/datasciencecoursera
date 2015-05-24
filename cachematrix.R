## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function takes a matrix object as input
# If no matrix is supplied as formal argument then it creates a null 
# matrix as input to the function
# This function returns below explained setter functions as return value

makeCacheMatrix <- function (x = matrix()) {
  
        #m_inv_matrix is assigned null value as first time the 
        #inverse of the matrix is solved in other function
        m_inv_matrix <- NULL
        
        # set function is used to set the value of input matrix or 
        # reset the m_inv_matrix i.e. calculated matrix to null
        # such that it will need to be recalculated
        
        set <- function(y) {
                x <<- y
                m_inv_matrix <<- NULL
        }
        
        #get function is imply used to return the value of input matrix
        #not the inverted matrix and mostly will be used for the first time
        #before the inverse is calculated
        
        get <- function () {
                x
        }
        
        #set_inverse function is called from other function which has calculated
        #the inverse of the matrix and wants to store it in the object
        #so that the matrix value can be reused <<- operator is used to assign
        #values of one object to another
        
        set_inverse <- function(v_inverse) {
                m_inv_matrix <<- v_inverse
        }
        
        #get_inverse function is always called after the inverse of the matrix 
        #is calculated by other function, bascially this function reads the
        # cached value of the matrix which is made persistent by set_inverse ()
        
        get_inverse <- function () {
                m_inv_matrix
        }
        
        #Below list of setter functions are called and returned to the CacheSolve 
        #function retruned as list object and individual functions are called
        
        list (set = set, get = get,setinverse = set_inverse,getinverse = get_inverse)
}




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