## The 'cachematrix.R' file creates and/or stores a special
## "matrix" object and returns the calculated inverse matrix

## The 'makeCacheMatrix' function builds a set of functions and
## returns the functions within a list to the parent environment

makeCacheMatrix <- function(x = matrix()) { #   'x' is initialized by user input function argument as a matrix vector
    inv <- NULL #                               'inv' is given NULL value to be intitialized below
    set <- function(y) {
        x <<- y #                               assigns 'y' to parent environment object 'x'
        inv <<- NULL #                          clears previous cached 'inv'; assigns NULL value to parent environment 'inv' object
    }
    get <- function() x #                       'x' is undefined and therefore retrieved from parent environment
    setInv <- function(invert) inv <<- invert # 'm' is defined in parent environment; 'vect' is an anonymous function assigned to 'inv'
    getInv <- function() inv #                  'inv' is retrieved from parent environment
    list(set = set, #                           allows use of $ extract operator; gives the name 'set' to the set() function defined above
         get = get, #                           gives the name 'get' to the get() function defined above
         setInv = setInv, #                     gives the name 'setmean' to the setmean() function defined above
         getInv = getInv) #                     gives the name 'getmean' to the getmean() function defined above
}

## The 'cacheSolve' function populates and/or retrieves the
## inverse matrix from the function/object 'makeCacheMatrix'

cacheSolve <- function(x, ...) { #              'x' is initialized with '...' to allow the caller to pass additional arguments
    ##
    ## Returns a matrix that is the inverse of 'x'
    ##
    inv <- x$getInv() #                         retrieves inverse matrix from 'getInv' as the argument passed into the 'inv' input object
    if(!is.null(inv)) { #                       skip if 'inv' is NULL; otherwise, retrieves 'inv' value stored in cache and returns it to the parent environment
        message("getting cached data") #        message indicating data is retrieved from cache
        return(inv) #                           prints retrieved cache data
    }
    vect <- x$get() #                           retrieves 'get' extracted from 'x' as the argument passed into the 'vect' input object
    inv <- solve(vect, ...) #                   calculates an inverse matrix
    x$setInv(inv) #                             uses the setInv() function on the input object to set the mean in the input object
    inv #                                       returns the value of the mean to the parent environment by printing the mean object
}
