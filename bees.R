#library(dplyr)
#library(tidyr)
library(raster)
library(e1071)
library(doParallel)
library(foreach)

main.path <- "C:/Users/m509652/Downloads/Bees Data"
train.path <- paste0(main.path, "/images/train")
test.path <- paste0(main.path, "/images/test")

# I. We already have a training set; create a CV set that's 25% of the 
# training set
# ==============================================================================
getCVNames <- function(path){
        
        
        # Chang code below
        # Read training label set and create CV out of that!
        # Assign to fnames the training labels (no need to sort)
        train.names <- read.csv(paste0(main.path,"/","train_labels.csv"), header = T,
                           colClasses = c('numeric','numeric'))
        
        
        # Randomly pick 25% indices from training for cross validation
        randInd <- runif(nrow(train.names)*0.25, min=1, max=nrow(train.names))
        cvnames <- train.names[randInd, ]
        
        #remove cv examples from train
        train.names <- train.names[!(train.names$id %in% cvnames$id),]
        
        
}


# ==============================================================================
# II. Function that reads in JPGs as Rasters, and performs dimensonality
# reduction on each image.
## For each JPG, it unlists the pixel RGBs and creates a matrix of training
## examples with pixel intensities as features
### m is the number of training images (70% of original)
### n is size xloc*yloc*pixeldepth;
### features are laid out as: x_pixeldepth(1)...y_pixeldepth(2)...z_pixeldepth(3)
### so, for rgb intensity, pixel depth is 3, for VGA it is 65,534!
### --------------------------------------------------------------
### Function then calls PCA to find the Principal Components of most correlated
### colors (features). It reduces dimensions with a variance retention tolerance
### specified in 'tol'. Ideally, we want to retain 99% of variance.
# ==============================================================================

# names is a vector of names, path is a string pointing to folder path
readImgs <- function(names, path){
        
        # TRAIN OR CV
        #read in names of images (train or cv).
        train.imgs <- matrix(0, nrow=1, ncol=200*200*3)
        system.time(
        train.imgs <-  t(sapply(train.names$id, function(X){
                temp <- getValues(brick(paste0(train.path,"/",paste0(X,".jpg"))))
                c(temp[,1], temp[,2], temp[,3])
        }))
        )
        
        
        # CV
        cv.imgs <- matrix(0, nrow=1, ncol=200*200*3)
        system.time(
                cv.imgs <-  t(sapply(cvnames$id, function(X){
                        temp <- getValues(brick(paste0(train.path,"/",paste0(X,".jpg"))))
                        c(temp[,1], temp[,2], temp[,3])
                }))
        )
        
        
        
        
        # TEST
        test.imgs <- matrix(0, nrow=1, ncol=200*200*3)
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