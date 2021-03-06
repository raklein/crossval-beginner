################################################################################
# Cross-validation tutorial for absolute beginners in R
# by Rick Klein
# R beginners: start from the top
# Everyone else, skip to the start of the cross-validation portion
################################################################################

# Text following the '#' symbol is "commented out", meaning these lines don't
# actually run, they are only there for explanation.

# I recommend installing and opening this script in R Studio.
# If you have problems running this script locally, there's a web
# version you can run here: https://rstudio.cloud/project/66369

# If you open the .Rproj file, that will automatically set your working
# directory (e.g., it will tell R to look for files in the same folder as the
# .Rproj file). In other scripts, you may have to do this manually with a 
# setwd() command. 

# What you're reading (.R file) is an r script. This is similar to the "syntax"
# window of SPSS. If you type all your code here, you can go back later 
# and re-run it. This saves you a ton of time and allows your analysis to be
# reproducible by others (and yourself!). Always type code you _may_ want to
# save in a script! I very rarely use the console (window below, where the 
# code is actually executed).

# To execute or 'run' a line, highlight what you want to run and press 'Run'
# on the bar above. 

# Try that on the line below:
2 + 2

# You should see it output this in the console window below: 
# > 2 + 2
# [1] 4

# First, it indicates the code that was run, in this case 2 + 2. 
# Then, it outputs the result: 4.
# The part saying [1] is just an index in case there are many rows of results.
# In this case, it is saying "4" is the first result (and here it's the only
# result).

# Note that if you run a line, R will only display the output (not save it).
# If we want to save this result, we can tell R to do that like this:
result_example <- 2 + 2

# This saves the result of 2 + 2 to the new object "result_example". Note,
# now we have the opposite problem - it doesn't print that output. We can print
# the object by simply calling it by name:
result_example

# You can see a full list of the objects you've saved in this session by 
# looking at the "Environment" window. 

# A concept that will be more important later is that objects in R have 
# different types. This is similar to SPSS where different columns can be
# numeric, or string, etc. We can check the type of result_example with:
class(result_example)

# class() is a function. You will use a ton of these in R. It performs some
# operation on the object or commands inside the parentheses. In this case,
# it tells us result_example is a numeric object.

# To learn more about any function, you can type ? followed by the function
?class

# This will invoke the "help" window, and describe what the function does
# and also what arguments it takes (e.g., what goes inside the parentheses).
# These help windows can be pretty confusing, don't be afraid to Google too.

# To clean up our workspace for the rest of the tutorial, I'll delete the
# example object we just created
rm(result_example)

# Ok, these were just some bare basics, and now we'll move on to the demos
# on cross-validation (holdout and K-folds). If this is your first experience
# with R, you will likely get lost at some points. 
# The most important thing is to try to get the jist of what is
# happening, so don't worry if you don't understand every little bit of code.
# That said, don't heisitate to ask questions about even the most basic thing!

################################################################################
################################################################################
# Start of cross-validation tutorial
################################################################################
################################################################################

# First I'm going to load the 'packages' I'll be using. Packages are basically
# collections of functions you can import and use in R. By default, base R
# can't do too much, its usefulness rests on people developing packages
# for it.

# The first time you use a package you have to install it. After that,
# it's a good practice to just comment out these lines. If you don't
# have these packages, remove the "#" so these install.packages lines run.

#install.packages("tidyverse") # Collection of useful packages I always load.
#install.packages("caret", dependencies = TRUE) # only needed for k-folds example.

# You also have to "load" the package into your active R session. You must
# do this every time you start R. This is kind of a pain in the butt to do
# manually, so just remember to always include a set of library calls
# for all packages you use in the script.
library("tidyverse")
library("haven") # haven installs with tidyverse, but you have to load it separately.
# Haven is used for reading spss/sas/stata data.
library("caret")

##TROUBLESHOOTING INSTALL:
# If either of the above fail to install or load: 
# 1) Update R (https://cran.r-project.org/) and R Studio (https://www.rstudio.com/products/rstudio/download/)
# to the latest versions (update R first, then R studio), restart your computer, and re-try.
# 2) If you get an error like "Error: package or namespace load failed for 'ggplot2'"
# then install any indicated package separately. In the above example you would
# run install.packages("ggplot2").

# Let's load a sample dataset. Here, we're assigning the result of the read_sav()
# call to the object 'data'. read_sav() function reads in a .sav file
# from SPSS. Inside read_sav we simply put the relative location of the dataset.
# In this case, it's contained under a subdirectory called "data".
data <- read_sav("./data/raceiat_2017.sav")

# I'm going to subset to remove incomplete sessions for simplicity
data <- subset(data, session_status == "C")

# These are publicly available data from the Race IAT on Project Implicit.
# From here: https://osf.io/gwofk/ however to save space I took only the 
# first 5000 rows.

# We can do just a little inspection of this data to get a feel for it:
glimpse(data)

# If these data don't make sense, there is also a codebook in the folder
# For the moment, key variables to consider:
# D_biep.White_Good_all = participant IAT score. Higher=greater preference 
# for white people.
# att7 = explicit item asking racial preference. Higher = greater preference
# for white people.

## Holdout dataset (or "split-halves") approach
# Here, we're simply trying to randomly split the data into a 'train' dataset
# to perform exploratory analyses, and then a 'test' dataset to verify performance.

# Before we do so, recall that this will involve randomization. Randomization
# is the enemy of reproducibility, you will get different numbers each time. 
# In R, we fix this by setting a "seed", which forces R to use the same
# randomization. Note that the number inside set.seed(1) can be changed, to
# test how robust your particular randomization is. 
set.seed(1)

# Here's a simple method to randomize train and test samples. This
# exact method is modified from one DataCamp uses, but there are dozens
# of ways to code this same procedure.
rows <- sample(nrow(data)) #creates randomized vector same length as data
data_randomized <- data[rows,] #randomizes df to index from 'rows'
split <- round(nrow(data)*.80) #creates index to split the file into 4/5 1/5, rounded
train <- data_randomized[1:split,] #first 4/5 to train
test <- data_randomized[(split+1):nrow(data),] #remaining 1/5 to test

# Now, a very basic thing we could do is use the first partition of the data
# to explore (e.g., check moderators, look for patterns) and then when
# we think we've found something, we confirm that pattern in the unused partition

# For example, a simple linear regression predicting IAT score from explicit race attitudes, using the 'train' data partition:
summary(lm(data=train,D_biep.White_Good_all~att7))

# This shows a significant positive relationship, as we might expect.

# Does this hold on the 'test' data?
# (you might consider pre-registering before this step)
summary(lm(data=test,D_biep.White_Good_all~att7))

# Unsurprisingly we observe the same trend. This suggests that the pattern
# we discovered in the first half of the data was probably not some fluke,
# and we weren't capitalizing on fitting a model to idiosyncratic noise only 
# present in the first set of data (e.g. "overfitting" the model)

# This isn't a great example of the protective power against false-positives
# because I only ran a very basic model. But if you included several moderators,
# and had flexibility in your data analysis, this is one way to test if you've
# found a real effect or are capitalizing on chance.

# Now, this isn't true cross-validation just yet. This is basically just a 
# mini-replication study using the same data. We've only examined if a pattern
# we observe in the first partition is also present in the second, confirmatory
# partition (in this case, a significant, positive association between IAT and
# explicit attitudes). 

# To truly _predict_ we need to generate estimates about out-of-sample
# observations (e.g., if a person's IAT score is XX, their explicit atitude is YY).

# For linear regression, you can do that like this:

# Build the same model from above:
model1 <- lm(data=train,D_biep.White_Good_all~att7)
summary(model1) # same model, so same results

p <- predict(model1, test) # note the "test" dataset must have all the columns used in train

# This gives us a vector of point predictions for each observation in the test
# set. Whereas last time, we let the parameters of the regression change when
# we re-fit the model on the confirmatory partition, here we are actually
# predicting point-estimates for the out-of-sample data using the model we
# built on the training data.

# To evaluate how well this model predicts, we need to define error in some way.
# Basically, define how we are going to evaluate our performance.
# Here's one common metric:
error <- p - test$D_biep.White_Good_all # difference between our predicted IAT scores and the real scores
sqrt(mean(error^2)) # computing RMSE (root mean squared error) which is generally a better definition

# Note this returns NA. Why? Because at least one IAT score is NA, so it can't 
# complete the mean() call successfully. One solution is to simply ignore
# NA cases by adding na.rm = TRUE as an argument to the mean() function.

# HOWEVER, in general you should be careful about doing these sorts of 
# data-processing steps to only the train or test set (instead do it to both in
# advance before building your model). It risks allowing "leakage" between the
# training and test sets, invalidating the confirmatory test, and may also
# create differences between the training and test data that decreases
# performance of the model. I do it here for simplicity:
sqrt(mean(error^2, na.rm=TRUE))

# How do we interpret this error term? It's a little hard on its own. The 
# key intution to take away is that we have _some_ measure of error that 
# we want to reduce. This allows us to re-tool our model and evaluate other
# models against this one (the smaller the prediction error, the better
# the performance of the model)

## K-folds example
# Now, rather than a true hold-out sample, another popular option is
# k-folds validation. Basically, we repeat the train/test procedure 
# multiple times and average the results.

# The below script does 5 folds of validation. method = "lm" indicates
# we're using linear models, the same as we did in the hold-out example.

model2 <- train(
  D_biep.White_Good_all ~ att7, data,
  method = "lm", na.action = na.omit,
  trControl = trainControl(
    method = "cv", number = 5,
    verboseIter = TRUE
  )
)

print(model2)

# Note that neither form of cross-validation actually changes your algorithm.
# It only provides an estimate of out-of-sample performance. In fact,
# at the end of k-folds the model discards the 'folds' and
# re-fits the model using ALL of the data. Here's a longer explanation of this
# common misconception: https://stats.stackexchange.com/questions/96026/final-model-prediction-using-k-fold-cross-validation-and-machine-learning-method

# Note that the train() function we just used is part of the caret package.
# Although we didn't do any machine learning in this script, the caret 
# package is a beginner package for machine learning. We could adapt
# the above code to do many different kinds of machine learning pretty simply.
# Not so bad!
# At this point you're ready for some more advanced practice, try the other tutorials
# or experiment with this code by perhaps trying to find other variables that are better
# predictors of implicit attitudes. Or, use your own data (this script should be fairly flexible)

# Also, this code structure follows closely with the methods taught in
# this DataCamp course: www.datacamp.com/courses/machine-learning-toolbox
# you could easily pick up from here by following that course and get right
# into machine learning.