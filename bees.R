#library(dplyr)
#library(tidyr)
library(raster)


path <- "C:/Users/M509652/Downloads/Bees Data/Images/train"
pathCV <- "C:/Users/M509652/Downloads/Bees Data/Images/cv"

# I. We already have a training set; create a CV set that's 25% of the 
# training set
# ==============================================================================
CVset <- function(path, pathCV){
        
        
        # Chang code below
        # Read training label set and create CV out of that!
        # Assign to fnames the training labels (no need to sort)
        
        
        
        # Get # of files in folder
        fnames <- list.files(path, pattern="*.jpg", recursive = F)
        
        # Randomly pick 25% indices of total training set
        randInd <- runif(length(fnames)*0.25, min=1, max=length(fnames))
        CVnames <- fnames[randInd]

        # Move these files only to cross validation folder using file.rename
        lapply(CVnames, function(X) {
                # change this in the future to file.copy; renames messing up the index
                # i think; isn't moving the exact % of files i want
                file.rename(paste0(path,"/",X), paste0(pathCV,"/",X))
        })
        
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

readImages <- function(fnames){
        
        #read by composing fnames with ".jpg"
        
        train <- data.frame()
        system.time(
        train <- lapply(fnames, function(X){
                temp <- getValues(brick(paste0(path,"/",X)))
                rbind(train, as.data.frame(cbind(t(temp[,1]),t(temp[,2]),t(temp[,3]))))
                
        }))
        
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