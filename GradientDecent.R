##Source("")

#data.set.dir <- Sys.glob(file.path( "*", "data" , "txt")

spam <- fread("spam.data.txt")
SAheart <- fread("SAheart.data 2.txt")
zip.train<- fread("zip.train.data.txt")
#### Formatting Input/Output Matrices
spam.y <- spam[, V58]
spam.X <- spam[, c(1:ncol(spam)) ]

SAheart.y <- SAheart[, "chd" ]
SAheart.X <- SAheart[, c(1:10)]


## !!! zip.x rows != zip.y rows ( off by one)
zip.y <- zip.train[V1 == 0 | V1 == 1, 1]
zip.y[V1 == 0 | V1 == 1]
zip.x <- zip.train[ V1 == 0 | V1 == 1 , c(2:99)]


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