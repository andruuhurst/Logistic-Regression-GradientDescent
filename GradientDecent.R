##Source("")
library(data.table)

# Read in data sets from files

spam <- scale(fread("data/spam.data.txt"))

# randomize 

set.seed(10)
spam <- spam[ sample(nrow(spam)),] 

# split data sets

spam.train.size <- as.integer(.6 * nrow(spam))
spam.test.size <-  as.integer(spam.train.size + (.2 * nrow(spam)))

spam.train <- spam[c(0:spam.train.size),]
spam.test <- spam[ c((spam.train.size + 1) : spam.test.size),]
spam.val <- spam[ c((spam.test.size+1) : nrow(spam)),]

#### Formatting Input/Output Matrices

spam.train.y <- spam.train[, ncol(spam)]
spam.train.X <- spam.train[, c(1:ncol(spam)-1) ]

spam.test.y <- spam.test[, ncol(spam)]
spam.test.X <- spam.test[, c(1:ncol(spam)-1) ]

spam.val.y <- spam.val[, ncol(spam)]
spam.val.X <- spam.val[, c(1:ncol(spam)-1) ]

### create alt labels ( y hat )
 # spam
spam.train.y[spam.train.y > 0] <- 1
spam.train.y[spam.train.y < 0] <- -1

spam.val.y[ spam.val.y > 0 ] <- 1 
spam.val.y[ spam.val.y < 0 ] <- -1 

spam.test.y[ spam.test.y > 0 ] <- 1 
spam.test.y[ spam.test.y < 0 ] <- -1 


#### Gradient Decent fucntion ####

## test values
X <- matrix(spam.train.X , nrow(spam.train.X) , ncol(spam.train.X))
y <- matrix(spam.train.y)
X.val <- matrix( spam.val.X , nrow(spam.val.X) , ncol(spam.val.X))
stepSize <- .001
maxIteration <- 100

##

GradientDescent <-function( X , y , stepSize , maxIterations){
  
  ### intialize
    # initialize var wieghtVector ( intiialize at zero vector / size of features)
    weightVector <- matrix(rep( 0 , ncol(X)))
  
    # initialize var weightMatrix of real numbers
     # (number of rows = number of input features, number of columns = maxIterations)
    weightMatrix <- weightVector
    
  # for loop through 1 to max iterations
    maxIteration.vec <- seq(1 , maxIteration, 1)
    
    for( i in maxIteration.vec ){
      
      # compute gradient given current weightVector
        # use function compute gradient over all trianing data
      
      grad = -(y[i,] %*%  X[i,]) / ( 1 + exp( y[i,] *  as.numeric(t(weightVector) %*% X[1,])))
      
      
      #update the weightVector by taking a step in the negative gradient decent
      weightVector <- weightVector + t( .01 * grad )
      
      #store in the resulting weightVector in corresponding col of weightMatrix
      weightMatrix <- cbind(weightMatrix, weightVector)
                            
    }
   
  weightMatrix <- weightMatrix[ c( 1 : nrow(weightMatrix)) , c( 2 :ncol(weightMatrix))]    
  #at the end return algorithm  
  return (weightMatrix)
   
}

  ## call Gradient Decent funciton on trian data to compute a learned weightMatrix

   
  ## multiply train and validation inputs with weightMatrixto obtain matrix of prediction values
    # # of rows = observations , # of cols = number iterations



 #### GRAPHS  #######

   ## Plot error rate ( % incorrectly predicted labels) & log loss as function of # iterations

   PrecentError <- function( maxIterations, step, X , Y){
     
     ### compute weightMatrix with size of iterations from 1 to max
     iter.vec <- seq(1 , maxIteration, 1)
     weightMatrix.vec <- vector()
     
     for( i in iter.vec){
        err.list <- list()
        wm<-GradientDescent( X , y , step , i )
        pred.m <- wm %*% X
     }
     
     
     
     ### compute the error rate for each size of weight matrix
     
     ### return all error rates as a function of iterations
   } 

     ## include legend or direct labels for differentiation

  ## plot a point to highligh the minimum value of each of the two curves

  ## find number of iterations that minimizes teh validation error 


  ## make a table of error rates with three rows ( train/validation/ test sets )
   ## two cols ( lgo regression and baseline)



  ## for each model ( log Regression and baseline ) compute ROC

  ## for each model plot a circle/dot in the same color that shows the FPR/TPR