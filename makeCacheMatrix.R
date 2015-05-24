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

