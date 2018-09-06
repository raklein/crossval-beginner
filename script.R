################################################################################
# Cross-validation tutorial for absolute beginners in R
# by Rick Klein
# R beginners: start from the top
# Everyone else, skip to the start of the cross-validation portion
################################################################################

# Text following the '#' symbol is "commented out", meaning these lines don't
# actually run, they are only there for explanation.

# I recommend installing and opening this script in R Studio

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
# it's a good practice to just comment out these lines.
install.packages("tidyverse") # Collection of useful packages I always load

# You also have to "load" the package into your active R session. You must
# do this every time you start R. This is kind of a pain in the butt to do
# manually, so just remember to always include a set of library calls
# for all packages you use in the script.
library("tidyverse")
library("haven") # haven installs with tidyverse, but you have to load it separately.
# Haven is used for reading spss/sas/stata data.

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
# test how sensitive your particular randomization is. 
set.seed(1)

# Here's a simple method to randomize train and test samples. This
# exact method is the one DataCamp recommends, but there are dozens
# of ways to code this same procedure.
rows <- sample(nrow(data)) #creates randomized vector same length as data
data_randomized <- data[rows,] #randomizes df to index from 'rows'
split <- round(nrow(data)*.80) #creates index to split the file into 4/5 1/5, rounded
train <- data_randomized[1:split,] #first 4/5 to train
test <- data_randomized[(split+1):nrow(data),] #remaining 1/5 to test

# Now, we could simply dvelop our model on the train half and once we think
# we've found something, run the identical model on the test split to see 
# if it holds up. 

# For example, a simple linear regression predicting IAT score from explicit race attitudes:
summary(lm(data=train,D_biep.White_Good_all~att7))

# This shows a significant positive relationship, as we might expect.
# Does this hold on the test data? (ideally, you pre-register before this step)
summary(lm(data=test,D_biep.White_Good_all~att7))

# Unsurprisingly it holds, although some variability in the effect size.
# Note this is the only model I ran so this is not really demonstrating
# the protective power against p-hacking and false-positives. 

# We also just repeated our same model, we didn't predict and test values.
# You can do that like this:

# Build the same model from above:
model1 <- lm(data=train,D_biep.White_Good_all~att7)
summary(model1) # same model, so same results

p <- predict(model1, test)

# This gives us a vector of point predictions for each observation in the test
# set. Note this is an out-of-sample prediction - we're making predictions about
# novel (to the computer, at least) data.

# To give some idea for how well we did, we need to define error:
error <- p - test$D_biep.White_Good_all # difference between our predicted IAT scores and the real scores
sqrt(mean(error^2)) # computing RMSE (root mean squared error) which is generally a better definition

# Note this returns NA. Why? Because at least one IAT score is NA, so it can't complete
# the mean() call successfully. One solution is to simply ignore NA cases by adding 
# na.rm = TRUE as an argument to the mean() function.
sqrt(mean(error^2, na.rm=TRUE))

# HOWEVER, in general you should avoid doing data-processing steps like this to only
# the train or the test set. It risks allowing "leakage" between the training and test
# sets, invalidating the "test" part, and may also create differences between
# the training and test data that decreases performance of the model. A better method
# would be to go back and anticipate this problem starting with our training data.