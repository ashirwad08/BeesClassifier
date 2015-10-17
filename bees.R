#library(dplyr)
#library(tidyr)
library(raster)


path <- "/Users/ash/Downloads/Bees Data"

# I. We already have a training set; create a CV set that's 25% of the 
# training set
# ==============================================================================
CVnames <- function(path){
        
        
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
        
#         
#         # Get # of files in folder
#         fnames <- list.files(path, pattern="*.jpg", recursive = F)
#         
#         
#         # Move these files only to cross validation folder using file.rename
#         lapply(CVnames, function(X) {
#                 # change this in the future to file.copy; renames messing up the index
#                 # i think; isn't moving the exact % of files i want
#                 file.rename(paste0(path,"/",X), paste0(pathCV,"/",X))
#         })
        
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

trainSet <- function(tnames){
        
        #read in tnames images. This is the training set.
        train <- matrix(0, nrow=1, ncol=200*200*3)
        system.time(
        train <-  t(sapply(tnames$id, function(X){
                               temp <- getValues(brick(paste0(path,"/images/train/",paste0(X,".jpg"))))
                               c(temp[,1], temp[,2], temp[,3])
                               }))
        )
        
        
        # Don't forget to cbind response at the end!!
        #train <- cbind(train, tnames$genus)
}


# ===============================================================================
## Dimensionality Reduction
## Will use PCA with 95% variance retention (first run)
# ===============================================================================
reduceDims <- function(train){
        
        # About an hour to perform PCA on approx. 3100 x 120000 matrix with 
        # 99% variance retention!!!
        system.time(
                red.img <- prcomp(train, center = T, scale. = F, tol = 0.01)
        )
        
        # Diagnostics on PCs:
        # Screeplot shows most variation is contained in about 100 PCs
        # Cummulative distribution plot against PCs shows that 95% of the 
        # dataset's images variance can be explained by a lower 1150 dimensional
        # manifold. So, now to create the lower dimensional training set, I'll
        # multiply the 1150 PCs (eigenvectors) with the training matrix to get
        # the corresponding projections onto the lower subspace.
        # m x k = m x n (dataset) * n x k (selected eigenspace)
        final.train <- train %*% red.img$rotation[,1:1150]
        
        
}










# ==============================================================================
# III. This is a classification problem, depending on many features we're able
# to reduce to, we want to use Logistic Regression because the number of features
# will probably be greater than the number of available training examples, 
# assuming we want to retain most of the variance.
# 
# Can also implement a neural network to compare results.
# ==============================================================================
train(final.dat)