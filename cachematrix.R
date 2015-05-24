# This makeCacheMatrix function has all required functionalities such as checking, setting, 
# storing, and returning the inverse of any given input matrix. First it checks the incoming 
# matrix and if the input matrix is a new matrix, sets it. When proper function is called it 
# updates the cache with the inverse of it. If the incoming matrix is same as the previous matrix, 
# it simply returns the inverse stored from cache.  


makeCacheMatrix <- function(x=matrix()){
        invmat<-NULL #set inverse matrix object to null initially
        set<-function(y){  
                if(!is.null(y)){ #checks if the incoming value is not null
                        if(identical(y,x))#checks if the incoming value same as the previous matrix, if yes, returns the inverse
                        {message("The input matrix is same as previous one, so getting cached inverse of matrix.")
                         return(invmat)}
                        else{ #it is a new matrix
                                x<<-y #assigns it to x
                                invmat<<-NULL #resets the cache
                                message("New matrix is set and here is the inverse for the new matrix.")}
                }
                else message("Input is null") 
        }      
        get<-function()x #this returns the input matix
        setsolve<-function(mat)invmat<<-mat #sets the inverse matrix to the cache
        getsolve<-function()invmat #returns the inverse from cache
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve) #returns list of makeCacheMatrix functions to the called object
}

# cacheSolve function checks whether the inverse matrix is already computed and stored, if not 
# computes and sends the inverse to store in the cache. This function has the second argument 
# with default NULL value. So it is optional. This way you can send the new input matrix via cacheSolve 
# itself. Or you can send the input matrix straightaway using set function in makeCacheMarix's object also. 
# Ex. x is old matrix, and y is new matrix
#
# cacheSolve1 <- makeCacheMatrix(x)
#
# cacheSolve(cacheSolve1,y) 
# or 
# cacheSolve1$set(y) 
# 
# Both methods work. 

cacheSolve <- function(x,y=NULL,...){   
        if(!is.null(y)) 
                x$set(y)  #if the second argument is provided, sends the input matrix to makeCacheMatrix
        else 
                invmat<-x$getsolve() #if the second argument is not provided, gets the cache value
        if(!is.null(invmat)){ #if cache has some value other than NULL, print the message and return the value which is going to be the inverse
                message("Getting cached inverse of matrix.")
                return(invmat)
        }
        
        data<-x$get() #if the cache is empty, get the input matrix
        invmat<-solve(data, ...) #calculate inverse of it
        x$setsolve(invmat)#store it in the cache
        invmat #print the inverse
}
