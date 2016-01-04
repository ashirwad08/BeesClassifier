#library(dplyr)
#library(tidyr)
library(caret)
library(raster)
library(rgdal)
library(e1071)
library(doMC)
library(doParallel)
library(foreach)

main.path <- "C:/Users/m509652/Downloads/Bees Data"
train.path <- paste0(main.path, "/images/train")
test.path <- paste0(main.path, "/images/test")

# I. We already have a training set; create a separate test set from it to get an 
# idea of the Out of Sample errors.
# ==============================================================================
getTestTrain <- function(path){
        
        #set.seed(3145)

        raw.names <- read.csv(paste0(main.path,"/","train_labels.csv"), 
                                header = T,
                           colClasses = c('character','numeric'))
        
        inTrain <- createDataPartition(raw.names$genus, p=0.7, list=F)
        train.names <- raw.names[inTrain,]
        test.names <- raw.names[-inTrain,]
        
        loadImages(train.names, test.names)
}


# ==============================================================================
# II. Function that reads in JPGs as Rasters
## For each JPG, it unlists the pixel RGBs and creates a matrix of training
## examples with pixel intensities as features
### m is the number of images
### n is size of each image= xloc*yloc*pixeldepth, for each pixel;
### so, for rgb intensity, pixel depth is 3, for VGA it is 65,534, etc!
# ==============================================================================

# names is a vector of names, path is a string pointing to folder path
loadImages <- function(train.names, test.names){
        
        cl <- makeCluster(4)
        # read train, cv, and submission test images in parallel
        registerDoParallel(cl, cores=detectCores())
        
        
        train.imgs <- matrix(0, nrow=1, ncol=200*200*3)
        
        system.time(
                train.imgs <- foreach(train.names$id, 
                                      .combine=rbind, 
                                      .inorder=TRUE,
                                      .export=c('getValues','brick')) %dopar% {
                                              temp <- getValues(brick(paste0(train.path,"/",paste0(train.names$id,".jpg"))))
                                              c(temp[,1], temp[,2], temp[,3])
                                      }
        )
        
        stopCluster(cl)
        
        
        
        
        
        # TRAIN 
        train.imgs <- matrix(0, nrow=1, ncol=200*200*3)
        system.time(
        train.imgs <-  t(sapply(train.names$id, function(X){
                temp <- getValues(brick(paste0(train.path,"/",paste0(X,".jpg"))))
                c(temp[,1], temp[,2], temp[,3])
        }))
        )
        
        
        # CV (split test set to validate against)
        cv.imgs <- matrix(0, nrow=1, ncol=200*200*3)
        system.time(
                cv.imgs <-  t(sapply(test.names$id, function(X){
                        temp <- getValues(brick(paste0(train.path,"/",paste0(X,".jpg"))))
                        c(temp[,1], temp[,2], temp[,3])
                }))
        )
        
        
        
        
        # TEST (submission set, unknown genera)
        predict.imgs <- matrix(0, nrow=1, ncol=200*200*3)
        system.time(
        test.imgs <- t(sapply(list.files(test.path), function(X){
                temp <- getValues(brick(paste0(test.path,X)))
                c(temp[,1], temp[,2], temp[,3])
        }))
        )
}


# ===============================================================================
## Dimensionality Reduction
## Will use PCA with 95% variance retention (for now)
# ===============================================================================
reduceDims <- function(raw.imgs, test.imgs){
        
        
#         cl <- makeCluster(2)
#         registerDoParallel(cl)
#         
#         # !!!WARNING!!! About an hour to perform PCA on approx. 3100 x 120000 
#         # matrix with 99% variance retention.
#         system.time(
#                 pcs <- parLapply(cl, list(raw.imgs, test.imgs), function(X){
#                         prcomp(X, center = T, scale. = F, tol = 0.01)
#                 })
#                 #foreach(i=1:2, .combine=c) %dopar% 
#                 #prcomp(raw.imgs, center = T, scale. = F, tol = 0.01)
#         )
#         
#         stopCluster(cl)
        
        system.time(
        train.pcs <- prcomp(train.imgs, center = T, scale. = F, tol = 0.01)
        )
        
        # Diagnostics on PCs:
        # Screeplot shows most variation is contained in about 100 PCs
        # Cummulative distribution plot against PCs shows that 99% of the 
        # dataset's images variance can be explained by a lower 1150 dimensional
        # manifold. So, now to create the lower dimensional training set, I'll
        # multiply the 1150 PCs (eigenvectors) with the training matrix to get
        # the corresponding projections onto the lower subspace.
        # m x k = m x n (dataset) * n x k (selected eigenspace)
        
        
        #pcs.imgs <- raw.imgs %*% pcs$rotation[,1:1150]
        
        # Optimized training set preserving 99% of cummulative variance
        major.pcs <- train.pcs$rotation[,which(summary(train.pcs)$importance[3,]<=0.95)]
        train <- train.imgs %*% major.pcs
        cv <- cv.imgs %*% major.pcs
        

}

# ==============================================================================
# III. This is a classification problem;
# If we've managed to reduced the number of features to an intermediate number 
# n between 1 and 1000, then we can use an SVM implementation along with a 
# Gaussian Kernel calculation. If there are many training examples (>50000) and
# not as many features then we're looking at logistic regression because SVM's 
# might run slow.
# 
# Can also implement a neural network to compare results.
# ==============================================================================

train <- function(train){
        # PCA has managed to reduce reduce the images to 1150 Principal Comps
        # while retaining around 95% of the variance between images. Since we 
        # have approximately 3100 training examples, we'll use an SVM implement
        # ation with a Gaussian Kernel computed.
        
        # Remember, tnames$genus corresponds to the training set classification.
        hyp <- as.data.frame(cbind(train, BeeType=(train.names$genus)))
        cv <- as.data.frame(cbind(cv, BeeType=(cvnames$genus)))
        #y <- as.factor(tnames$genus)
        
        
        # Going to use SVM implementation in e1701 package
        system.time(
        fit <- tune(svm, train.x=hyp[,1:1096],train.y = hyp[,1097], 
                    data=hyp, type = "C-classification", 
                   kernel = "radial", 
                   validation.x = cv[,1:1096], validation.y = cv[,1097],
                   ranges = list(gamma=10^(-5:-1),cost=10^(-2:4)))
        )
        
        prd <- predict(fit, cv)
        confusion <- table(pred = prd, groundtruth = cv[,1097])
}