##Source("")
library(data.table)

### Read in data sets from files ####

spam <- scale(fread("data/spam.data.txt"))
SAheart <- fread("data/SAheart.data 2.txt")
SAheart <- scale(SAheart[, colnames(SAheart) != "row.names" & colnames(SAheart) != "famhist" , with=FALSE])
zip.t<- fread("data/zip.train.data.txt")


#### randomize #####

set.seed(10)
spam <- spam[ sample(nrow(spam)),] 
SAheart <- SAheart[ sample(nrow(SAheart)),]
zip.t <- zip.t[sample(nrow(zip.t)),]

### split data sets  ###

#spam

spam.train.size <- as.integer(.6 * nrow(spam))
spam.test.size <-  as.integer(spam.train.size + (.2 * nrow(spam)))

spam.train <- spam[c(0:spam.train.size),]
spam.test <- spam[ c((spam.train.size + 1) : spam.test.size),]
spam.val <- spam[ c((spam.test.size+1) : nrow(spam)),]

# SAheart

SAheart.train.size <- as.integer(.6 * nrow(SAheart))
SAheart.test.size <-  as.integer(SAheart.train.size + (.2 * nrow(SAheart)))

SAheart.train <- SAheart[c(0:SAheart.train.size),]
SAheart.test <- SAheart[ c((SAheart.train.size + 1) : SAheart.test.size),]
SAheart.val <- SAheart[ c((SAheart.test.size+1) : nrow(SAheart)),]

# zip

zip.train.size <- as.integer(.6 * nrow(zip.t))
zip.test.size <-  as.integer(zip.train.size + (.2 * nrow(zip.t)))

zip.train <- zip.t[c(0:zip.train.size),]
zip.test <- zip.t[ c((zip.train.size + 1) : zip.test.size),]
zip.val <- zip.t[ c((zip.test.size+1) : nrow(zip.t)),]

#### Formatting Input/Output Matrices  #####

#spam
spam.train.y <- spam.train[, ncol(spam)]
spam.train.X <- spam.train[, c(1:ncol(spam)-1) ]

spam.test.y <- spam.test[, ncol(spam)]
spam.test.X <- spam.test[, c(1:ncol(spam)-1) ]

spam.val.y <- spam.val[, ncol(spam)]
spam.val.X <- spam.val[, c(1:ncol(spam)-1) ]

#SAheart

SAheart.train.y <- SAheart.train[, ncol(SAheart)]
SAheart.train.X <- SAheart.train[, c(1:ncol(SAheart)-1) ]

SAheart.test.y <- SAheart.test[, ncol(SAheart)]
SAheart.test.X <- SAheart.test[, c(1:ncol(SAheart)-1) ]

SAheart.val.y <- SAheart.val[, ncol(SAheart)]
SAheart.val.X <- SAheart.val[, c(1:ncol(SAheart)-1) ]


#zip

zip.train.y <- spam.train[, 1]
zip.train.X <- spam.train[, c(2:ncol(zip.t)) ]

zip.test.y <- spam.test[, ncol(spam)]
zip.test.X <- spam.test[, c(1:ncol(spam)-1) ]

zip.val.y <- spam.val[, ncol(spam)]
zip.val.X <- spam.val[, c(1:ncol(spam)-1) ]



### create alt labels ( y hat )  #####

# spam
spam.train.y[spam.train.y > 0] <- 1
spam.train.y[spam.train.y < 0] <- -1

spam.val.y[ spam.val.y > 0 ] <- 1  
spam.val.y[ spam.val.y < 0 ] <- -1 

spam.test.y[ spam.test.y > 0 ] <- 1 
spam.test.y[ spam.test.y < 0 ] <- -1 

# SAheart
SAheart.train.y[SAheart.train.y > 0] <- 1
SAheart.train.y[SAheart.train.y < 0] <- -1

SAheart.val.y[ SAheart.val.y > 0 ] <- 1 
SAheart.val.y[ SAheart.val.y < 0 ] <- -1 

SAheart.test.y[ SAheart.test.y > 0 ] <- 1 
SAheart.test.y[ SAheart.test.y < 0 ] <- -1 

# Zip



