#library(dplyr)
#library(tidyr)
library(raster)
library(e1071)

main.path <- "/Users/ash/Downloads/Bees Data"

# I. We already have a training set; create a CV set that's 25% of the 
# training set
# ==============================================================================
getNames <- function(path){
        
        
        # Chang code below
        # Read training label set and create CV out of that!
        # Assign to fnames the training labels (no need to sort)
        tnames <- read.csv(paste0(path,"/","train_labels.csv"), header = T,
                           colClasses = c('numeric','numeric'))
        
        
        # Randomly pick 25% indices from training for cross validation
        randInd <- runif(nrow(tnames)*0.25, min=1, max=nrow(tnames))
        cvnames <- tnames[randInd, ]
        
        #remove cv examples from train
        tnames <- tnames[!(tnames$id %in% cvnames$id),]
        
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

readImgs <- function(names){
        
        #read in names of images (train or cv).
        raw.imgs <- matrix(0, nrow=1, ncol=200*200*3)
        system.time(
        raw.imgs <-  t(sapply(names$id, function(X){
                               temp <- getValues(brick(paste0(path,"/images/train/",paste0(X,".jpg"))))
                               c(temp[,1], temp[,2], temp[,3])
                               }))
        )
}


# ===============================================================================
## Dimensionality Reduction
## Will use PCA with 95% variance retention (for now)
# ===============================================================================
reduceDims <- function(raw.imgs){
        
        # !!!WARNING!!! About an hour to perform PCA on approx. 3100 x 120000 
        # matrix with 99% variance retention!!!
        system.time(
                pcs <- prcomp(raw.imgs, center = T, scale. = F, tol = 0.01)
        )
        
        # Diagnostics on PCs:
        # Screeplot shows most variation is contained in about 100 PCs
        # Cummulative distribution plot against PCs shows that 95% of the 
        # dataset's images variance can be explained by a lower 1150 dimensional
        # manifold. So, now to create the lower dimensional training set, I'll
        # multiply the 1150 PCs (eigenvectors) with the training matrix to get
        # the corresponding projections onto the lower subspace.
        # m x k = m x n (dataset) * n x k (selected eigenspace)
        # 
        # *** WRITE NEW CODE *** To auto use the first k PCs based on cumm.
        # variance cutoff
        pcs.imgs <- raw.imgs %*% pcs$rotation[,1:1150]
        
        
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

train <- function(final.train){
        # PCA has managed to reduce reduce the images to 1150 Principal Comps
        # while retaining around 95% of the variance between images. Since we 
        # have approximately 3100 training examples, we'll use an SVM implement
        # ation with a Gaussian Kernel computed.

        # Remember, tnames$genus corresponds to the training set classification.
        hyp <- as.data.frame(cbind(final.train, BeeType=as.factor(tnames$genus)))
        #y <- as.factor(tnames$genus)
        
        
        # Going to use SVM implementation in e1701 package
        fit <- svm(BeeType~., data=hyp, type = "C-classification", 
                   kernel = "radial", cost = 1)
}

