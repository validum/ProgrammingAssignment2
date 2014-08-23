makeCacheMatrix <- function(x = matrix())
{
    #creates object that caches its matrix and inverse to another environment
    #includes methods to get/set matrix and inverse
    
    matrix_cache <<- x #cached matrix
    inv_cache <<- NULL # cache inverse, initially a null object
    set_matrix <- function(y = matrix())
    {
        #caches new matrix and sets inverse back to null
        matrix_cache <<- y
        inv_cahe <<- NULL
    }
    get_matrix <- function() matrix_cache #returns cached matrix
    get_inverse <- function() inv_cache #returns cached inverse
    set_inverse <- function(inv = matrix) inv_cache <<- inv #push new inverse
    
    #list of functions belonging to object
    list(set_matrix_func = set_matrix, get_matrix_func = get_matrix, 
         get_inverse_func = get_inverse, set_inverse_func = set_inverse)
}


cacheSolve <- function(x, ...) 
{
    #takes and object of makeCacheMatrix and returns the stored
    #inverse.  If the inverse does not exist, calculates it and
    #updates object
    
    inv <- x$get_inverse() #gets inverse from makeCacheMatrix object
    if (!is.null(inv))
    {
        #if inverse exists, return inverse
        message("This inversed was cached")
        return(inv)
    }
    
    #if inverse did not exist, calculate inverse, push to object and return inverse
    new_matrix <- x$get_matrix_func()
    new_inv <- solve(new_matrix)
    x$set_inverse_func(new_inv)
    message("This inverse was just calculated")
    new_inv
}
