## This library is to compute an inverse of a square matrix with the assumption 
##that the determinant is non-zero. The way this library is designed is to cache 
## the inverted matrix once computed. Hence, if the same matrix is required to compute its inverse
## with a loop, then caching the results is going to improve the time to complete the program

## Create a list of four elements which cache the inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function (inverse) i <<- inverse

        
        getinverse <- function () i
        list (set = set, get = get,
              setinverse = setinverse, getinverse = getinverse)

}


## Compute if the inverse is not computed, otherwise, call solve function to compute 
## the inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse ()
        if (!is.null (i)) {
                message ("Inverse is cached")
                return (i)
        }
        data <- x$get()
        i <- solve (data)
        x$setinverse (i)
        i
}

#testing a sample
z <- matrix(c(4,2,7,6), 2, 2)
solve (z)

#testing with caching function
cacheSolve(makeCacheMatrix (z)) 

#git SECTION
#downsteam from a fork git
#git clone https://github.com/aounihajar/ProgrammingAssignment2.

#create a new repo in githib
#git init
#git remote add origin https://github.com/aounihajar/ProgrammingAssignment2_modified.git
#get remote -v
#git pull origin master --allow-unrelated-histories
#git add .
#git commit -m "R Programming, week 3 comment"
#git push --set-upstream origin master

#uploading a file for a second time
#git add .
#git commit -m "R Programming, week 3 comment"
#git push --set-upstream origin master