
# I. We already have a training set; create a CV set that's 25% of the 
# training set
# ==============================================================================
CVset <- function(){
        
        # Get # of files in folder
        file.count <- 
        
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

raw.dat <- readImages('test path') 

# extract cross validation set (maybe 20% because we have more features)

final.dat <- principalTrain('training set', 'variation tolerance for pca')



# ==============================================================================
# III. This is a classification problem, depending on many features we're able
# to reduce to, we want to use Logistic Regression because the number of features
# will probably be greater than the number of available training examples, 
# assuming we want to retain most of the variance.
# 
# Can also implement a neural network to compare results.
# ==============================================================================
train(final.dat)