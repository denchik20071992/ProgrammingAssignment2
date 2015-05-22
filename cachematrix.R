## Put comments here that give an overall description of what your
## functions do

## create a structure of the matrix function

makeCacheMatrix <- function(x = matrix()) {
        ## create a null value of the inverse matrix
        matrix_inv<-NULL
        set<-function(y){
                x<<-y
                ## if the matrix was changed, change the value of inverse matrix
                matrix_inv<<-NULL
        }
        ##recieve the value of the inverse matrix
        get<-function() x
        setmatrix_inv<-function(solve) matrix_inv<<- solve
        ## get the inverse matrix
        getmatrix_inv<-function() matrix_inv
        list(set=set, get=get,
             setmatrix_inv=setmatrix_inv,
             getmatrix_inv=getmatrix_inv)
}


## recieve the cash of the matrix

cacheSolve <- function(x, ...) {
        matrix_inv<-x$getmatrix_inv()
        if(!is.null(matrix_inv)){
                message("getting cached data")
                return(matrix_inv)
        }
        #in case the inverse matrix not calculated earlier, first it is calculated and then retrieved
        matrix<-x$get()
        matrix_inv<-solve(matrix, ...)
        x$setmatrix_inv(matrix_inv)
        matrix_inv
        ## Return a matrix that is the inverse of 'x'
}
