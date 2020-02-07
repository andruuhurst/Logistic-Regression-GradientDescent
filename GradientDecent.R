##Source("")
library(data.table)

#data.set.dir <- Sys.glob(file.path( "*", "data" , "txt")

spam <- scale(fread("spam.data.txt"))
SAheart <- scale(fread("SAheart.data 2.txt"))
zip.t<- scale(fread("zip.train.data.txt"))

# randomize 

set.seed(10)
spam <- spam[ sample(nrow(spam)),] 
SAheart <- SAheart[ sample(nrow(SAheart)),]
zip.t <- zip.t[sample(nrow(zip.t)),]

# split data sets

spam.train.size <- as.integer(.6 * nrow(spam))
spam.test.size <-  as.integer(spam.train.size + (.2 * nrow(spam)))

spam.train <- spam[c(0:spam.test.size),]
spam.test <- spam[ c((spam.train.size + 1) : spam.test.size),]
spam.val <- spam[ c((spam.test.size+1) : nrow(spam)),]

#### Formatting Input/Output Matrices

spam.train.y <- spam.train[, 58]
spam.train.X <- spam.train[, c(1:57) ]

spam.test.y <- spam.test[, 58]
spam.test.X <- spam.test[, c(1:57) ]

spam.val.y <- spam.val[, 58]
spam.val.y <- 
spam.val.X <- spam.val[, c(1:57) ]

## other data 

SAheart.y <- SAheart[, "chd" ]
SAheart.X <- SAheart[, c(1:10)]


## !!! zip.x rows != zip.y rows ( off by one)
zip.y <- zip.t[V1 == 0 | V1 == 1, 1]
zip.X <- zip.t[ V1 == 0 | V1 == 1 , c(2:99)]


#### Gradient Decent fucntion

GradientDecent <-function( X , y , stepSize , maxIterations){
  
  ### intialize
    # initialize var wieghtVector ( intiialize at zero vector / size of features)
    weight.vec <- rep( 0 , ncol(X))
  
    # initialize var weightMatrix of real numbers
     # (number of rows = number of input features, number of columns = maxIterations)
    weightMatrix <- matrix( 0 , ncol(X), maxIterations)
    
  # for loop through 1 to max iterations
    maxIteration.vec <- seq(1 , maxIterations, 1)
    for( iteration in maxIteration.vec ){
      
      # compute gradient given current weightVector
        # use function comput gradient over all trianing data
      
     # gradient(f, x, centered = TRUE)
      
      #update the weightVector by taking a step in the negative gradient decent
  
      #store in the resulting weightVector in corresponding col of weightMatrix
    }
  #at the end return algorithm  
  return (weightMatrix)
   
}

  ### format y (labels)  from data sets
    ## spam output (last col)

    ## SAheart output (last col)

    ## zip.trian ( first col , ignore classification that is not 1 or 0)

  ## scale the inputs 

  ## randomly split teh data into 60% train, 20% validation , 20% test


  ## call Gradient Decent funciton on trian data to compute a learned weightMatrix

   
  ## multiply train and validation inputs with weightMatrixto obtain matrix of prediction values
    # # of rows = observations , # of cols = number iterations



 #### GRAPHS

   ## Plot error rate ( % incorrectly predicted labels) & log loss as function of # iterations

     ## include legend or direct labels for differentiation

  ## plot a point to highligh the minimum value of each of the two curves

  ## find number of iterations that minimizes teh validation error 


  ## make a table of error rates with three rows ( train/validation/ test sets )
   ## two cols ( lgo regression and baseline)



  ## for each model ( log Regression and baseline ) compute ROC

  ## for each model plot a circle/dot in the same color that shows the FPR/TPR