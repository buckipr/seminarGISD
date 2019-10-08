#-----------------------------------------------------------------------------#
# deeper_data_prep.R                                                          #
#                                                                             #
# Clean data in deeper_data1.csv & create new variables                       #
# (a little plotting at the end)                                              #
#                                                                             #
# created: 2019-09-14  -jt                                                    #
# updated: 2019-09-17  -jt                                                    #
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# 0: BASIC COMMANDS
#-----------------------------------------------------------------------------#

# 0.1: Set up Working Directory
date()      ## print the current date and time at the Console
getwd()     ## print the current working directory at the Console
dir()       ## print the names of files/folders in the current directory
dir("../")  ## print the contents in the folder "above" the current one


dir.create("C:/Users/Student/Chewbacca") ## create a new folder 
setwd("C:/Users/Student/Chewbacca")      ## set the working directory
dir()

## What if the folder already exists?!?

# 0.2: Creating Objects
ls()      ## what objects are in R's memory?

x <- 3                 ## create a new object called "x" that holds the value 3
x                      ## print the content(s) / value(s) by evaluating the object's name
y <- c(5, -2, 0.2)     ## create another object by concatenating 3 numbers
y
z <- c("red", "green") ## we can use characters (or strings) as well
z
ls()


## these objects are actually vectors
is.vector(x); is.vector(y); is.vector(z)  ## see :)

## and the elements have certain attributes
class(x)
class(z)
class(c(x, y, z))  ## Ugh, that was horrible.  NEVER do that again!

## finally, if you wanted to remove an object, say z, 
rm(z)
ls()


# 0.3: Check Librararies and Access Help Files
library() ## list the libraries (or packages) that are installed on your computer
library(base)
library(Yoda)

library(help = Base)  ## oops!
library(help = base)

help.search("create directory")
help.search("directory")
help("file.exists")  ## can also use: ?file.exists (but not always as helpful)
file.exists("C:/Users/Student/Chewbacca/introR/")

# 0.4: Exercises

## [1] Create a new object/vector, called X, that holds the numbers for 1 to 10.

## [2] Create a new vector, called X_log, that is equal to the log of X.

## [3] Create 4 new vectors that hold letters a, b, c, and d,  Call the new vectors: 
##     (1) _new_vector, (2) 4timesX, (3) new_vector, and (4) new.vector

## [4] Explain to your neighbor what that horrible command does, why it returns [1] "character",
##     and why it is such a horrible example.  note: the horrible command is: class(c(x, y, z))  

## [5] HELP! What is going on here, why does R return an error for the last line?
n <- 3
n
n <- n + n
n
3 + N

## [Bonus] write an if statement that only creates the folder C:/Users/Student/Chewbacca/Temp
##         if it does not already exist.


#-----------------------------------------------------------------------------#
# 1: PREPARE EXAMPLE DATA SET
#-----------------------------------------------------------------------------#

# 1.0: Load Original Data (download from website) 
data1 <- read.csv("deeper_data1.csv") ## data on US States
class(data1)                          ## data frames can contain different types of variables
names(data1)                          ## print variable names
edit(data1)                           ## open up spreadsheet-like editor (careful!)
                                      #### Look, but don't touch PLEASE!!!!
dim(data1)                            ## dimensions (i.e., # of cases & variables)

# 1.1: Explore & Clean Variables

# 1.1.1: Variable: data1$X (state names)
## we access variables in data frames with $
## "X" as a variable name is not helpful!
data1$X

data1$stateName <- data1$X  ## let's preserve the original data and create a new variable
names(data1)
data1$stateName

length(data1$stataName)     ## huh?!?

## index a vector (matrix/data frame) using [element number] ([row, column])
data1$stateName[1]
data1[1, 1]

## quick data checks
data1$stateName == "Ohio"         ## did Ohio make it in?
table(data1$stateName == "Ohio")  ## a little easier to digest as a table
#### remember, R is an object oriented language!!!

which(data1$stateName == "Ohio")  ## which element (row) is it?
data1$stateName[35]

#### PG-23 (X-rated?)
data1$stateName[which(data1$stateName == "Ohio")]
data1$stateName[data1$stateName == "Ohio"]


unique(data1$stateName)          ## last I checked, there should be 50
length(unique(data1$stateName))  ## how many unique values do we have
                                 ## uh-oh! (code is getting a little scary again)

## Exercises

#### [1] What "type" of variable is stateName? What does this mean?

#### [2] Write an R command that prints out the 7th, 12th, and 33rd state names

#### [3] Write an R command that sorts the state names in reverse alphabetical order

#### [Bonus] Create a new variable that contains the state names, but in lower case letters

#### [Bonus * 5,000] Write an R command that prints out the state names that begin with the letter I


# 1.1.2: Variable: data1$Population
names(data1)
## start with a summary
summary(data1$Population)  ## "NA" is how R denotes a missing value

## negative numbers?!? hmm.... again, let's create a new variable to
## preserve original data (or we could create a new data frame)
data1$cleanPop <- data1$Population
data1$cleanPop == data1$Population  ## are these two vectors the same?

## to clean this variable, we need:
## (1) a way to index all the negative values
## (2) reassign new values to a state if it has a negative value
cbind(data1$stateName, data1$Population < 0)  ## cbind() concatenates vectors into columns
                                              ## rbind() does the same by rows

## hmmm...so much for our "helpful" version of state name.
## cbind() creates a matrix which (unlike a data frame) can only contain one type of variable
is.numeric(cbind(data1$stateName, data1$Population < 0))
data1[ , c(1, 2)]

## when R encounters what it thinks is a categorical measure
## (like strings) it creates a _FACTOR_ (we'll come back to these later)
is.factor(data1$stateName)
levels(data1$stateName)        ## print the "levels" (i.e., labels) 
is.ordered(data1$stateName)    ## ordinal factor? nope

cbind(as.character(data1$stateName), data1$Population < 0)

cbind(levels(data1$stateName), data1$Population < 0)

## ok, so ```data1$Population < 0``` will serve as an index for negative
## population sizes.  now we want to reassign new values, say -99,
## to our new variable ```data1$cleanPop```
data1$stateName[data1$Population < 0]
data1$cleanPop[data1$Population < 0] <- -99

cbind(levels(data1$stateName), data1$cleanPop, data1$Population < 0)

## maybe -99 as missing is not such a good idea (R will think it is a
## numeric value).  let's go with NA (R's way of denoting missing values)
data1$cleanPop[data1$Population < 0] <- NA

cbind(as.character(data1$stateName), data1$cleanPop, data1$Population < 0)

#### all done?
summary(data1$cleanPop)

## Exercises

#### [1] Create a new, cleaned version of the variable data1$Illiteracy

#### [2] Create a new, cleaned version of the variable data1$Life.Exp

#### [3] Create a new, cleaned version of data1$HS.Grad

#### [Bonus * 3] Clean the variable data1$Income
####         hint: original variable is a factor, that needs to be converted to NUMERIC values

# 1.2: Recode & Transform Variables
## bigPop as a yes/no variable
data1$bigPop <- NA
cbind(levels(data1$stateName), data1$cleanPop, data1$bigPop)

## remember the steps: create an index, then reassign values
data1$bigPop[data1$cleanPop >  1000] <- TRUE
data1$bigPop[data1$cleanPop <= 1000] <- FALSE

## sanity check
cbind(as.character(data1$stateName), data1$cleanPop, data1$bigPop)

table(data1$bigPop)
table(data1$bigPop, useNA="ifany") ## useNA="always" is another option

## logPop
data1$logPop <- log(data1$cleanPop)
summary(data1$logPop)

# 1.3: Plots & Figures
plot(density(data1$logPop)) ## ugh!

### a useful function for identifying missing/NA values:
is.na(data1$logPop)   ## ooh, and a useful index
!is.na(data1$logPop)

plot(density(data1$logPop[!is.na(data1$logPop)])) ## yah!
dev.off()

### who doesn't love a pretty picture
plot(density(data1$logPop[!is.na(data1$logPop)]),
     main="My first R plot",     ## Title
     xlab="log population size", ## label x-axis
     ylab="density")             ## label x-axis
abline(v=mean(data1$logPop[!is.na(data1$logPop)])) ## add vertical line
abline(v=mean(data1$logPop[!is.na(data1$logPop)]),
       col="red",  ## color
       lwd=5,      ## line width
       lty=3)      ## line type
dev.off()

### and a few more....
par(mfrow=c(2,2), mar=c(1,1,1,1))
#### top left
plot(density(data1$logPop[!is.na(data1$logPop)]),
     main="My first R plot",     ## Title
     xlab="log population size", ## label x-axis
     ylab="density",             ## label x-axis
     type="p",                   ## type of plot (p = points; l = lines; b = both, n = none)
     pch=1,                      ## plotting character
     cex=3,                      ## size of plotting character
     col="green")
#### top right
plot(density(data1$logPop[!is.na(data1$logPop)]),
     main="My second R plot",    ## Title
     xlab="log population size", ## label x-axis
     ylab="density",             ## label x-axis
     type="p",                   ## type of plot (p = points; l = lines; b = both, n = none)
     pch="w",                    ## plotting character
     cex=.3,                     ## size of plotting character
     col="red")
#### bottom left
plot(density(data1$logPop[!is.na(data1$logPop)]),
     main="My third R plot",     ## Title
     xlab="log population size", ## label x-axis
     ylab="density",             ## label x-axis
     type="l",                   ## type of plot (p = points; l = lines; b = both, n = none)
     lty=5,                      ## line type
     col="blue")                 ## color
#### bottom right
plot(density(data1$logPop[!is.na(data1$logPop)]),
     main="My fourth R plot",    ## Title
     xlab="log population size", ## label x-axis
     ylab="density",             ## label x-axis
     type="b",                   ## type of plot (p = points; l = lines; b = both, n = none)
     lty=3,                      ## line type
     col="grey",                 ## color
     pch=4)                      ## plotting character
### turn off device (i.e., close plotting window)
dev.off()

### we can also save the plot as a pdf:
par(mfrow=c(1,1))

pdf(file = "newPlot.pdf")
plot(density(data1$logPop[!is.na(data1$logPop)]),
     main="My first R plot",     ## Title
     xlab="log population size", ## label x-axis
     ylab="density",             ## label x-axis
     type="p",                   ## type of plot (p = points; l = lines; b = both, n = none)
     pch=1,                      ## plotting character
     cex=3,                      ## size of plotting character
     col="green")
dev.off()

### Boxplot
boxplot(data1$logPop[!is.na(data1$logPop)] ~ data1$bigPop[!is.na(data1$logPop)])
dev.off()

## try to capture 3 variables
summary(data1$HS.Grad)
data1$cleanHS <- NA
data1$cleanHS[data1$HS.Grad < 100] <- data1$HS.Grad[data1$HS.Grad < 100]
summary(data1$cleanHS)

data1$cleanE0 <- NA
data1$cleanE0[data1$Life.Exp > 6 & data1$Life.Exp < 9999] <- data1$Life.Exp[data1$Life.Exp > 6 & data1$Life.Exp < 9999]
summary(data1$cleanE0)

#### Example with symbols
plot(data1$cleanHS, data1$cleanE0, type="n", main="Example with symbols()",
     xlab = "% of Pop with HS Degree", ylab = "Life Expectancy")
symbols(x=data1$cleanHS, y=data1$cleanE0, circles=data1$cleanPop)
dev.off()

#### Example with identify & mtext
plot(data1$cleanHS, data1$cleanE0, type="n", main="",
     xlab = "% of Pop with HS Degree", ylab = "Life Expectancy")
symbols(x=data1$cleanHS, y=data1$cleanE0, circles=data1$cleanPop)
mtext(text="Example with identity()", side=3, line=3)
mtext(text="click on circle to reveal name (Esc to stop)", side=3, line=2)
identify(x=data1$cleanHS, y=data1$cleanE0, labels=data1$stateName)
dev.off()

#### Example with text
plot(data1$cleanHS, data1$cleanE0, type="n", main="Example with text()",
     xlab = "% of Pop with HS Degree", ylab = "Life Expectancy")
text(x=data1$cleanHS, y=data1$cleanE0, labels=data1$stateName,
     cex = .6 + data1$cleanPop/max(data1$cleanPop, na.rm=TRUE))
dev.off()

## barplot
table(data1$bigPop)
barplot(table(data1$bigPop), col=c("red", "grey"))
dev.off()

#---------------------------------------------------------------------#
# 99: ALL DONE
#---------------------------------------------------------------------#
save.image("deeper_data1_clean.RData")
