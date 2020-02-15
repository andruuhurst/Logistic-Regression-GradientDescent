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
spam.input.list <- list(  )
spam.train.y <- spam.train[, ncol(spam)]
spam.train.X <- spam.train[, c(1:ncol(spam)-1) ]
spam.input.list$train <- matrix(spam.train.X , nrow(spam.train.X) , ncol(spam.train.X))

spam.test.y <- spam.test[, ncol(spam)]
spam.test.X <- spam.test[, c(1:ncol(spam)-1) ]
spam.input.list$test <- matrix(spam.test.X ,nrow(spam.test.X) , ncol(spam.test.X) )

spam.val.y <- spam.val[, ncol(spam)]
spam.val.X <- spam.val[, c(1:ncol(spam)-1) ]
spam.input.list$val <- matrix(spam.val.X , nrow(spam.val.X) ,  ncol(spam.val.X) )

### create alt labels ( y hat )
 # spam
spam.output.list <- list()

spam.train.y[spam.train.y > 0] <- 1
spam.train.y[spam.train.y < 0] <- -1
spam.output.list$train <- matrix(spam.train.y  )

spam.val.y[ spam.val.y > 0 ] <- 1 
spam.val.y[ spam.val.y < 0 ] <- -1 
spam.output.list$val <- matrix(spam.val.y )

spam.test.y[ spam.test.y > 0 ] <- 1 
spam.test.y[ spam.test.y < 0 ] <- -1 
spam.output.list$test <- matrix(spam.test.y )
#### Gradient Decent fucntion ####

## test values
X <- matrix(spam.input.list$train , nrow(spam.input.list$train) , ncol(spam.input.list$train))
y <- matrix(spam.output.list$train)
X.val <- matrix( spam.input.list$val , nrow(spam.input.list$val) , ncol(spam.input.list$val))
stepSize <- .1
maxIteration <- 999

##

GradientDescent <-function( X , y , stepSize , maxIterations){
  
  ### intialize
    # initialize var wieghtVector ( intiialize at zero vector / size of features)
    weightVector <- matrix(rep( 0 , ncol(X)))
  
    # initialize var weightMatrix of real numbers
     # (number of rows = number of input features, number of columns = maxIterations)
    weightMatrix <- weightVector
    
  # for loop through 1 to max iterations
    maxIteration.vec <- seq(1 , maxIteration , 1)
    
    for( i in maxIteration.vec ){
      
      # compute gradient given current weightVector
        # use function compute gradient over all trianing data
      
      grad = -(y[i,] %*%  X[i,]) / ( 1 + exp( y[i,] *  as.numeric(t(weightVector) %*% X[i,])))
      
      
      #update the weightVector by taking a step in the negative gradient decent
      weightVector <- weightVector - t( .01 * grad )
      
      #store in the resulting weightVector in corresponding col of weightMatrix
      weightMatrix <- cbind(weightMatrix, weightVector)
                            
    }
   
  weightMatrix <- weightMatrix[ c( 1 : nrow(weightMatrix)) , c( 2 :ncol(weightMatrix))]    
  #at the end return algorithm  
  return (weightMatrix)
   
}

  ## call Gradient Decent funciton on trian data to compute a learned weightMatrix
  wm <- GradientDescent(X , y , stepSize , maxIteration)
  
   
  ## multiply train and validation inputs with weightMatrixto obtain matrix of prediction values
    # # of rows = observations , # of cols = number iterations
  pred.m <- list()
  pred.m$train <- X %*% wm
  pred.m$val <- X.val %*% wm

  graph.list <- list()
  
  metric.funs <- list(
    mean.log.loss = function( pred.mat , y.hat){
      colMeans( log( 1 + exp(-t(y.hat) %*% pred.mat)))
    },
    error.rate = function( pred.mat , y.hat){
      pred.mat <- ifelse(pred.mat < 0 , -1 , 1)
      100 * colMeans( y.hat != pred.mat )
    })
  
  for( split in c( "train" , "val")){
   for( fun.name in names(metric.funs) ){
     this.fun <- metric.funs[[fun.name]]
     metric.vec <- this.fun( pred.m$split , spam.output.list$split )
     graph.list[[paste(split , fun.name)]] <- data.table(
       split , fun.name , value = metric.vec ,
       iteration = seq_along(metric.vec)
     )
   } 
  }
  
  graph.dt <-do.call(rbind , graph.list)
  min.dt <- metric.dt[ , data.table( what = "min" , .SD[which.min(value)]) ,
                       by = .(split , fun.name)]
  
  ggplot() + geom_line( aes( iteration , value , color = split ) , data = graph.dt) +
    geom_point( aes( iteration , value , color = split , fill=what), shpae = 21, data= min.dt) +
    scale_fill_manual(values=c(min="white"))+
    scale_color_manual(values=c(train="black", val="red"))+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(fun.name ~ ., scales="free")
  
  
    
  
 #### GRAPHS  #######

   ## Plot error rate ( % incorrectly predicted labels) & log loss as function of # iterations
     
     ### compute weightMatrix with size of iterations from 1 to max
     
   
     ## include legend or direct labels for differentiation

  ## plot a point to highligh the minimum value of each of the two curves

  ## find number of iterations that minimizes teh validation error 


  ## make a table of error rates with three rows ( train/validation/ test sets )
   ## two cols ( lgo regression and baseline)



  ## for each model ( log Regression and baseline ) compute ROC

  ## for each model plot a circle/dot in the same color that shows the FPR/TPR