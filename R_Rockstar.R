
# 1.  Shows the working Directory

getwd()

# 2. Changes the Working Directory. Set the default working directory to be the one where you have saved your datasets

setwd("C:/myfolder/data")

# 3. The Global Environment. The global environment is the interactive workspace. This is the environment where one actually works





####################################################################################################################################
# DescriptiveStats.R


### Open csv file purchaseOrder.csv ####

purchase_order <- read.csv(file.choose(), header = TRUE)

# first 6 rows
head(purchase_order) 

# create a variable cost (data from cors_per_order column)
cost <- purchase_order$Cost_per_order

### mean ####
mean(cost)

# median ####
median(cost)


### mode ####
# Create the function
getmode <- function(x) {
  uniqvalue <- unique(x)
  uniqvalue[which.max(tabulate(match(x, uniqvalue)))]
}

# apply function to cost
getmode(cost)

# apply function to a column with nominal data
# purchase_order$Supplier  - means column "Supplier" 
# from a data table "purchase_order" - Note the dollar sign
getmode(purchase_order$Supplier)

# let's try a histogram
library(ggplot2)

ggplot(data= purchase_order, aes(Supplier)) + 
  geom_bar(stat = "count")


## adjust axis labels
ggplot(data= purchase_order, aes(Supplier)) + 
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## hjust can be 0 or 1


library(plotly)

## create a variable plot
p <- ggplot(data= purchase_order, aes(Supplier)) + 
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplotly(p)

### column names

colnames(purchase_order)

### Task 1: Find the mode for AP_Terms_Months

### Midrange ####

minimum <- min(cost)
maximum <- max(cost)

midrange <- (minimum + maximum)/2


### Range ####
p-range <- maximum - minimum


## Task 2
## Compute the rand for AP_Terms_Months


### Quantiles #####
quantile(cost)

IQR(cost)

### Variance and Standard Deviation####

var(cost)

sd(cost)

library(qcc)
qcc(cost, type="xbar.one", std.dev = "SD")
## if you get an error about margins run the following line 92 or 93
x11() # for mac
windows() # for pc


qcc(purchase_order$AP_Terms_Months, type="xbar.one", std.dev = "SD")

### useful functions ###
summary(purchase_order)

str(purchase_order)


### Z-Score ###
#generate z-scores for variable A using the scale() function

scale(cost, center = TRUE, scale = TRUE)

hist(cost)
x11()
hist(scale(cost, center = TRUE, scale = TRUE))

### Coefficient of Variation

1/(sd(cost, na.rm = TRUE)/ mean(cost, na.rm = TRUE))

## covariance ####

# between AP_Terms_Months and Item_Cost
attach(purchase_order)

cov(AP_Terms_Months,Item_Cost)

cor(AP_Terms_Months,Item_Cost)
### scatter plot 
ggplot(purchase_order, aes(x=Item_Cost, y=AP_Terms_Months)) +
  geom_point(shape=1) 

ggplot(purchase_order, aes(x=Item_Cost, y=AP_Terms_Months)) +
  geom_point(shape=1) + geom_smooth() # # Add a loess smoothed fit curve with confidence region



#### Solutions:
# Solution 1
ggplot(data= purchase_order, aes(AP_Terms_Months)) + 
  geom_bar(stat = "count") # answer = 30
## Solution 2
minimum <- min(purchase_order$AP_Terms_Months)
maximum <- max(purchase_order$AP_Terms_Months)
maximum-minimum # answer = 30







#######################################################################################################################################

# An example of using the Quartz editor capability
elements <- data.frame()
elements <- edit(elements)
print(elements)
# The code below is not working for me, but you can try it
#
# x <- read.table(file="clipboard", sep = "\t", header = TRUE)
# x
#
# reading from a csv file (create one local on your laptop)
# replace the source file location below to your local system
#
fromcsv <- read.csv("~/Desktop/R-Work/data/elementdata.csv")
str(fromcsv,vec.len=2)  # note that it converts string to factors
# turning off the factoring
fromcsv2 <- read.csv("~/Desktop/R-Work/data/elementdata.csv", stringsAsFactors = FALSE)
str(fromcsv2,vec.len=2)
#
fromcsv3 <- read.table(file="~/Desktop/R-Work/data/elementdata.csv", sep=",", header = TRUE)
fromcsv3
#
# writing data
#
# to csv file
x <- data.frame(a = I("a \" quote"), b=pi)
write.table(x, file="~/Desktop/foo.csv", sep=",", col.names = NA, qmethod = "double")
#
# read file back into R
#
read.table("~/Desktop/foo.csv", header = TRUE, sep=",", row.names = 1)
#
# can also use .csv functions
#
write.csv(x, file="~/Desktop/foo2.csv")
read.csv("~/Desktop/foo2.csv", row.names=1)
# without row names
#
write.csv(x, file = "~/Desktop/foo3.csv", row.names = FALSE)

# dealing with duplicates
#
duplicated(iris)
which(duplicated(iris))
iris[!duplicated(iris),]
#
# Removing incomplete data rows
str(airquality)
complete.cases(airquality)
x <- airquality[complete.cases(airquality),]
str(x)
#  remember there are other ways
#
x <- na.omit(airquality)
str(x)
#
# Calculated fields
#
x <- iris$Sepal.Length / iris$Sepal.Width
head(x)
#
# working with sets of data 
head(state.x77)
#
frost <- state.x77[,"Frost"]
head(frost,5)
x <- cut(frost, 3, include.lowest=TRUE)
x
x <- cut(frost, 3, include.lowest=TRUE, labels=c("Low","Med","High"))
x
# determine counts
table(x)
#
# merging data
all.states <- as.data.frame(state.x77)
str(all.states)
all.states
all.states$Name <- rownames(state.x77)
str(all.states)
all.states
row.names(all.states) <- NULL
str(all.states)
all.states
#
#
# Extracting dataframes
cold.states <- all.states[all.states$Frost > 150, c("Name","Frost")]
str(cold.states)
cold.states
# 
large.states <- all.states[all.states$Area > 10000, c("Name","Area")]
str(large.states)
large.states
#
# Merge the data together
merge(cold.states,large.states)
merge(cold.states,large.states, all=TRUE)
# can create lookup tables
index <- match(cold.states$Name, large.states$Name)
index
large.states[na.omit(index),]
#
# using %in% instead
index <- cold.states$Name %in% large.states$Name
index
cold.states[index,]
#
# Sorting
some.states <- data.frame(Region=state.region, state.x77)
str(some.states)
some.states <- some.states[1:13, 1:3]
some.states
# Sorting vectors
sort(some.states$Population)
# Sort passing optional argument
sort(some.states$Population, decreasing = TRUE)
# Sorting dataframes
# vector received as ordered indexes
order.pop <- order(some.states$Population)
order.pop
# output list of populations by index
some.states$Population[order.pop]
# output of the dataframe rows based on order
some.states[order.pop,]
#
# sort by multiple columns
index <- with(some.states, order(Region, Population))
some.states[index,]
# Using apply() function, rows or vector, function
str(Titanic)
apply(Titanic, 1, sum)
apply(Titanic, 2, sum)
apply(Titanic, 3, sum)
apply(Titanic, c(3,4), sum)
#
# Use lapply() and sapply()
#
lapply(iris, class)
# sapply()
#
sapply(iris, class)
sapply(iris, mean)
sapply(iris, function(x) if(is.numeric(x)) mean(x) else NA)
#
# tapply()
tapply(iris$Sepal.Length, iris$Species, mean)
with(iris, tapply(Sepal.Length, Species, mean))
#
# Higher dimensional tables
str(mtcars)
cars <- transform(mtcars, am = factor(am, levels = 0:1, labels = c("Automatic","Manual")))
str(cars)
with(cars, tapply(mpg, am, mean))
with(cars, tapply(mpg, list(gear, am), mean))
# Using aggregate function
#
with(cars, aggregate(mpg, list(gear = gear, am = am), mean))
#
#
# Data handling part 2 starts here...
#
#
install.packages("reshape2")
library("reshape2")
# create some data
games <- c("1st","2nd","3rd","4th")
venues <- c("Bruges","Ghent","Ghent","Bruges")
granny <- c(12,4,5,6)
geraldine <- c(5,4,2,4)
gertrude <- c(11,5,6,7)
# create dataframe using our data
goals <- data.frame( Game = games, Venue = venues, Granny = granny, Geraldine = geraldine, Gertrude = gertrude)
goals
mgoals <- melt(goals) # convert to long
mgoals
mgoals <- melt(goals, id.vars = c("Game","Venue")) # setting id vars explicitly
mgoals
# think of the formula's below as a way of producing a 'pivot table'
dcast(mgoals, Game + Venue ~ variable, sum)
dcast(mgoals, variable ~ Venue, sum)
dcast(mgoals, Venue ~ variable, sum)
dcast(mgoals, Venue + variable ~ Game, sum)
#
install.packages("ggplot2")

library("ggplot2")      ### another issue i have locally, plot does not appear in right window
ggplot(mgoals, aes(x = variable, y = value, fill = Game)) + geom_bar(stat="identity")
       
       
       
       
       
       
       
       
       
#####################################################################################################################################
       
  # Start Script.R
       
 source("~/Desktop/R-Work/Scripts/runSampleFuncs.R")



######################################################################################################################################
       
# RunSampleFunc.R
       
 
       source("~/Desktop/R-Work/Scripts/exampleFuncs.R")
percentages <- c(58.23, 120.4, 33)
print("### Function call single argument ###")
print(addPercent(percentages))

print("### Function call multiple arguments ###")
print(addPercent2(percentages, mult = 1))

print(priceCalculator(hours=55))
print(priceCalculator(hours=110))

print("### Function call default values")
print(priceCalculator2(25, public = TRUE))
print(priceCalculator2(25, public = FALSE))
print(priceCalculator2(25))

priceCalculator2(25)
priceCalculator2(110)

priceCalculator2(c(25,100))


print("### Example with nested if/else statements ###")
tmpData <- data.frame(hours=12, type='private')
print( with(tmpData, priceCalculator3a(tmpData$hours, client=tmpData$type)))


print("### Updated call using ifelse for vectors ###")
client <- data.frame(hours = c(25,110,125,40), public = c(TRUE,TRUE,FALSE,FALSE))
print("### showing use of the 'with' function")
print( with(client, priceCalculator3(hours, public = public)) )

print("### Function with for loop ###")
client$type <- c("public","abroad","private","abroad")


print( with(client, priceCalculator4(client$hours,client = client$type)))

print("### Function with pre-allocated vector")
print( with(client, priceCalculator5(client$hours, client=client$type)))

print("### Showing use of the matrix iterator sapply")
counts <- matrix(c(3,2,4,6,5,1,8,6,1),ncol=3)
colnames(counts) <- c("sparrow","dove","crow")
counts
apply(counts,2,max,na.rm=TRUE)

sapply(c("a","b"), switch, a = "hello", b = "Goodbye")

print("### Showing use of function using sapply instead of for loop ###")
print( with(client, priceCalculator6(client$hours, client=client$type)))
       
       
       
       
#################################################################################################################################
       
 # ExampleFUnc.R

       
### below is code which will automatically be executed upon a 'source' command
x <- c(0.458, 1.6653, 0.83112)
percent <- round(x * 100, digits=1)
result <- paste(percent, "%", sep = " ")
print(result)

### code below is loaded upon a 'source' command but only executed when called
# function with single argument
addPercent <- function(x) {
  percent <- round(x * 100, digits = 1)
  result <- paste(percent, "%", sep = " ")
}
# multiple arguments
addPercent2 <- function(x, mult) {
  percent <- round(x * mult, digits = 1)
  result <- paste(percent, "%", sep = " ")
  return(result)
}
# if condition
priceCalculator <- function(hours, pph=40) {
  net.price <- hours * pph
  if(hours > 100) {
    print("in here")
    net.price <- net.price * 0.9
  }
  round(net.price)
}
# if/else conditions
priceCalculator2 <- function(hours, pph=40, public=TRUE) {
  net.price <- hours * pph
  if(hours > 100) net.price <- net.price * 0.9
  if(public) {
    tot.price <- net.price * 1.06
  } else {
    tot.price <- net.price * 1.12
  }
  round(tot.price)
}
# if used with vectors
priceCalculator3 <- function(hours, pph = 40, public) {
  net.price <- hours * pph
  net.price <- net.price * ifelse(hours > 100, 0.9, 1)
  tot.price <- net.price * ifelse(public, 1.06, 1.12)
  round(tot.price)
}
# nested if/else 
priceCalculator3a <- function(hours, pph = 40, client) {
    
    net.price <- hours * pph
  
    if (client == "private") {
      tot.price <- net.price * 1.12 
    } else {
      if(client == "public") {
        tot.price <- net.price * 1.06
      } else {
        tot.price <- net.price * 1
      }
    }
    round(tot.price)
}
# for loop
priceCalculator4 <- function(hours, pph = 40, client) {
  net.price <- hours * pph * ifelse(hours> 100, 0.9, 1)
  VAT <- numeric(0)
  for(i in client) {
    VAT <- c(VAT, switch(i, private = 1.12, public = 1.06, 1))
  }
  tot.price <- net.price * VAT
  round(tot.price)
}
# for loop with pre-allocated vector
#
####  please note, the code below is slightly different than in the slidedeck
####  slidedeck has an incorrect line within the for loop
#
priceCalculator5 <- function(hours, pph = 40, client) {
  net.price <- hours * pph * ifelse(hours > 100, 0.9, 1)
  nclient <- length(client)
  VAT <- numeric(nclient)
  for(i in seq_along(client)) {
    # print(i)
    VAT[i] <- switch(client[i], private = 1.12, public = 1.06, 1)
  }
  tot.price <- net.price * VAT
  round(tot.price)
}
# for loop replaced with sapply
priceCalculator6 <- function(hours, pph = 40, client) {
  net.price <- hours * pph * ifelse(hours > 100, 0.9, 1)
  
  VAT <- sapply(client, switch, private = 1.12, public = 1.06, 1)
  
  tot.price <- net.price * VAT
  round(tot.price)
}


       
 ###############################################################################################################################
      #Trends.R
       
# https://datascienceplus.com/analyzing-google-trends-data-in-r/
#https://christophriedl.net/2013/08/22/google-trends-with-r/
# Load required libraries
library(gtrendsR)
library(reshape2)

library(RCurl)		# For getURL() and curl handler / cookie / google login
library(stringr)	# For str_trim() to trip whitespace from strings

# Google account settings
username <- "YOUR_NAME@gmail.com"
password <- "YOUR_PASSWORD"

username <- "obscrivn@gmail.com"
password <- "Site1-Dlya-Mami1."

# URLs
loginURL 		<- "https://accounts.google.com/accounts/ServiceLogin"
authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
trendsURL 		<- "http://www.google.com/trends/TrendsRepport?"

############################################
## This gets the GALX cookie which we need to pass back with the login form
############################################
getGALX <- function(curl) {
  txt = basicTextGatherer()
  curlPerform( url=loginURL, curl=curl, writefunction=txt$update, header=TRUE, ssl.verifypeer=FALSE )
  
  tmp <- txt$value()
  
  val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], val = TRUE)
  strsplit(val, "[:=;]")[[1]][3]
  
  return( strsplit( val, "[:=;]")[[1]][3]) 
}


############################################
## Function to perform Google login and get cookies ready
############################################
gLogin <- function(username, password) {
  ch <- getCurlHandle()
  
  ans <- (curlSetOpt(curl = ch,
                     ssl.verifypeer = FALSE,
                     useragent = getOption('HTTPUserAgent', "R"),
                     timeout = 60,         
                     followlocation = TRUE,
                     cookiejar = "./cookies",
                     cookiefile = ""))
  
  galx <- getGALX(ch)
  authenticatePage <- postForm(authenticateURL, .params=list(Email=username, Passwd=password, GALX=galx, PersistentCookie="yes", continue="http://www.google.com/trends"), curl=ch)
  
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful!")
  } else {
    print("Google login failed!")
  }
  return(ch)
}

ch <- gLogin( username, password )
authenticatePage2 <- getURL("http://www.google.com", curl=ch)

google.trends = gtrends(c("blu-ray"), gprop = "web", time = "all")[[1]]

google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date
google.trends$date = NULL
       
       
       
       
       
##############################################################################################################################
       # timeseries.R

       
 # http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
#the age of death of 42 successive kings of England
kings <- scan("https://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
#kings
# Store as time series
kingstimeseries <- ts(kings)

#  the number of births per month in New York city, from January 1946 to December 1959
births <- scan("https://robjhyndman.com/tsdldata/data/nybirths.dat")

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

souvenir <- scan("https://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))

plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
###
# additive model - The seasonal birth fluctuations are roughly constant in size over time and 
# do not seem to depend on the level of the time series, and the random fluctuations also 
# seem to be roughly constant in size over time
###
plot.ts(souvenirtimeseries)
####
# dditive model is not appropriate for describing this time series, 
# since the size of the seasonal fluctuations and random fluctuations seem 
# to increase with the level of the time series
###
#we may need to transform the time series in order 
#to get a transformed time series that can be described using an additive model
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)
###

### Moving Average ####
?filter

ma = filter(birthstimeseries, sides=2, c(rep(1,12)/12)) # 12 pt moving average

par(mfrow=c(2,1)) # A 2 by 1 panel of plots

plot(birthstimeseries, main="births")
#plot(ma, ylim=c(min(ma),max(ma)), main="moving average")
plot(ma, main="moving average")
# residuals
par(mfrow=c(1,1))
ts.plot(birthstimeseries, ma, lty=2:1, col=1:2, lwd=1:2, main="births with moving average superimposed")
residuals = births - ma
plot(residuals, main = "residuals from moving average")


       
       
       
#############################################################################################################################
       
       # Sampling.R
       
       
   #### Sampling, Subsetting and Pivot Table #####

head(mtcars)

### Selecting variables ####
myvars <- c("cyl","mpg")
newdata <- mtcars[myvars]

### Excluding variables ####
myvars.excl <- names(mtcars) %in% myvars
newdata <- mtcars[!myvars.excl]

### Selecting Observations #####
# first 20 observations
newdata <- mtcars[1:20,]  # 20 rows

newdata <- mtcars[, 1:3] # 3 columns

# based on variables
attach(mtcars)
#mtcars$mpg
newdata <- mtcars[which(cyl == 4), ] 
newdata <- mtcars[which(cyl == 4 & mpg < 30), ] 

# random sample of size n
# take a random sample of 10 mtcars
mysample <- mtcars[sample(1:nrow(mtcars), 10, replace=FALSE),]


#### Pivot table #####
payroll <- read.csv("CT-Payroll-FY2014.csv", stringsAsFactors=FALSE)
payroll <- read.csv(file.choose(),stringsAsFactors=FALSE )
View(payroll)

#This is just the first 10 rows of almost 96,000 state employees. That's a lot of data. Let's think about what this type of information can tell us.
depts <- table(payroll$AGENCY_DESCRIPTION)
depts <- depts[order(-depts)]
head(depts, 10)
#How many employees there are in each department
#What is the average or median pay by department

write.csv(depts, "depts.csv")

depts_avg <- tapply(payroll$TOTAL_AMT, payroll$AGENCY_DESCRIPTION, mean)
head(depts_avg[order(-depts_avg)], 10)

#Note: You can't do a pivot table analysis in Excel with median. You can in R
depts_median <- tapply(payroll$TOTAL_AMT, payroll$AGENCY_DESCRIPTION, median)
head(depts_median[order(-depts_median)], 10)

       
       
 ####################################################################################################################################
       
    # Regression.R
 
 ## linear regression

x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function.
relation <- lm(y~x)

print(relation)

print(summary(relation))

# Find weight of a person with height 170.
a <- data.frame(x = 170)
result <-  predict(relation,a)
print(result)

### plot
plot(y,x,col = "blue",main = "Height & Weight Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",
     ylab = "Height in cm")


#### Multiple regression

# Using mtcars data
head(mtcars)
input <- mtcars[,c("mpg","disp","hp","wt")]

# Create the relationship model.
model <- lm(mpg~disp+hp+wt, data = input)

# Show the model.
print(summary(model))


### Extract Coefficients
a <- coef(model)[1]
print(a)

Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]

print(Xdisp)
print(Xhp)
print(Xwt)

Y = a + Xdisp*x1 + Xhp*x2 + Xwt*x3


x1 <- 221
x2 <- 102 
x3 <- 2.91

Y = a + Xdisp*x1 + Xhp*x2 + Xwt*x3 # the predicted mileage 

#### Logistic Regression

# Select some columns form mtcars.
input <- mtcars[,c("am","cyl","hp","wt")]

mymodel = glm(formula = am ~ cyl + hp + wt, data = input, 
              family = binomial)

summary(mymodel)

       
       
####################################################################################################################################
       
       
# Plotting.R
       
       
 # http://www.cookbook-r.com/Graphs/
#http://www.r-graph-gallery.com/136-stacked-area-chart/
# Load the ggplot2 package
library(ggplot2)

### Bar with Values ####
dat <- data.frame(
  time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)
dat
#     time total_bill
# 1  Lunch      14.89
# 2 Dinner      17.23

# Basic bar graph
ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_bar(stat="identity")


# Map the time of day to different fill colors
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(stat="identity")


### Bar with Counts ####

library(reshape2)
### using data from the library reshape2
data(tips)

head(tips)

ggplot(data=tips, aes(x=day)) +
  geom_bar(stat="count")

### Adding Title and xlab, ylab

ggplot(data=tips, aes(x=day)) +
  geom_bar(stat="count") +
  ggtitle("Count per day") +
  xlab("Days of the Week") +
  ylab("Count of Bills")

### Line Graph ####

ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line()

## adding points
ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line() +
  geom_point() 

## change line and point
ggplot(data=dat, aes(x = time, y = total_bill, group = 1)) + 
  geom_line(colour = "red", size = 1.5) +
  geom_point(colour = "red", size = 4, shape = 21, fill = "white")

### Stacked Area Chart ###

# DATA

Sector <- rep(c("S01","S02","S03","S04","S05","S06","S07"),times=7)
Year <- as.numeric(rep(c("1950","1960","1970","1980","1990","2000","2010"),each=7))
Value <- runif(49, 10, 100)
data <- data.frame(Sector,Year,Value)
head(data)
ggplot(data, aes(x=Year, y=Value, fill=Sector)) + 
  geom_area()

### Scatter plot ####
# The iris dataset is proposed by R
head(iris)

# basic scatterplot
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point()

### Bubble Plot ####
data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")
head(data)

ggplot(data, aes(x = Women, y = Men)) +
  geom_point(aes(size=gap))

ggplot(data, aes(x = Women, y = Men,group = School)) +
  geom_point(aes(size=gap, colour=School))

### Heatmaps ####
library(plotly)

# Data: mtcars:
head(mtcars)
data=as.matrix(mtcars)

#basic heatmap
plot_ly(x=colnames(data), y=rownames(data), z = data, type = "heatmap")

# with normalization
data=apply(data, 2, function(x){x/mean(x)})
plot_ly(x=colnames(data), y=rownames(data), z = data, type = "heatmap")

### need to learn about APPLY - type ?apply
?apply

### Create word cloud ####
library(wordcloud2)

skills <- c("R","python","java","data","swim","golf","SQL","NLP")
weights <- c(5,2,3,6,3,2,1,6)

mydata <- data.frame(skills,weights)

wordcloud2(mydata,shape="circle")



?wordcloud2

#### Assignment - Make your own cloud #####


       
       
       
####################################################################################################################################
       
       #################Inference.R
       
    ### t-test ####
# http://statistics.berkeley.edu/computing/r-t-tests
x = rnorm(10) # data simulation from normal distribution
y = rnorm(10)
# Welch's t-test  adjusts the number of degrees of 
# freedom when the variances are thought not to be equal to each other
t.test(x,y)

ttest = t.test(x,y)

names(ttest)

# extract statistic
ttest$statistic
ttest[['statistic']]
ts = replicate(1000,t.test(rnorm(10),rnorm(10))$statistic)
# degree of freedom n1 + n2 -1 = 10+10-2= 18

# range
range(ts)
pts = seq(-4.177854,3.423646,length=100)
plot(pts,dt(pts,df=18),col='red',type='l')
#plot the theoretical density of the t-statistic, 
#and superimposing the density of our sample on top of it
lines(density(ts))

### Power of the t-test ####
#install.packages(pwr)
library(pwr)

# Task 1
### poooled standard deviation
s=sqrt(((n1 - 1)*s1^2+(n2-1)*s2^2)/(n1+n2 - 2))
# create values for n1, n2, s1, s2
# effect size
d <- (n1 - n2 )/s

### Task 2 ####
# What is the power of a one-tailed t-test, with a
# significance level of 0.01, 25 people in each group, 
# and an effect size equal to 0.75?
# use pwr.t.test

###Task 3 #####
# Using a two-tailed test proportions, and assuming a
# significance level of 0.01 and a common sample size of 
# 30 for each proportion, what effect size can be detected 
# with a power of .75?

### Plot power
# http://www.statmethods.net/stats/power.html

##### Answer Task 1######
n1 <- 1750
n2 <- 1612
s1 <- 89.93
s2 <- 69.05

# s = 80.59666
# d <- 1.7122

# Find the power of t-test
## Answer Task 2 ####
pwr.t.test(n=25,d=0.75,sig.level=.01,alternative="greater")
# power = 0.5988


## Answer Task 3 ####
pwr.2p.test(n=30,sig.level=0.01,power=0.75)
# h = 0.839


  ################################################################################################################################
       
  # datatypes (use of combine function)
#
snums <- c(1,2,3)
print(snums)
sints <- c(1L,2L,3L)
print(sints)
#
# Vector creatin
rep(c(1,10,2)) -> tmp
print(tmp)
# below replicates vector 4 times
rep(c(1,10,2),times=4) -> tmp
print(tmp)
#
#  Another way of doing assignment to a varible, which ever you use, be consistent within your code
j = "t"
j
# below replicates each member of the vector 4 times
rep(c(1,10,2),each=4) -> tmp
print(tmp)
#
# Factors
directions <- c("North", "East", "South", "West")
str(directions)
dfacts <- factor(directions,levels = c("North","East","South","West"),labels = c("N","E","S","W"))
nlevels(dfacts)
levels(dfacts)[1:2]
# table determines number of vector entries for each level
table(dfacts)
status <- c("Lo","Hi","Med","Med", "Hi")
ordered.status <- factor(status,levels=c("Lo","Med","Hi"), ordered = TRUE)
ordered.status
print(ordered.status)  # displays 'ranking'
table(ordered.status)
#
# Dates
xd <- as.Date("2016-11-16")
print(xd)
weekdays(xd)
xd+7
xd + 0:6
weekdays(xd + 0:6)
xm <- seq(xd, by = "2 months", length.out = 6)
xm
months(xm)
quarters(xm)
nd <- "Nov 17, 2016"  # this line and below useful for data transforms
newdate <- as.Date(nd, format="%b %d, %Y")
paste("Original date: ",nd," Converted to R: ",newdate)  # useful for combining values for display
#
#
# Time
test <- "June 29, 1958, 20:17:39"
test.fmt <- "%B %d, %Y, %H:%M:%S"
xct = as.POSIXct(test, format=test.fmt, tz="UTC")
xct
format(xct, "%d/%m/%y")
format(xct, "%M minutes past %I %p, on %d %B %Y")
xct + 7*86400
xct + 3*60*60
xct - 7*86400
xct
# operations
as.Date(xct)-7
Sys.time() < xct
early.start <- as.POSIXct("1940-01-01")
early <- seq(early.start, by = "10 years", length.out = 4)
early
early > xct  # recall vectoized operations
# extraction
xlt <- as.POSIXlt(xct)
xlt$year
xlt$mon
#
# matrix
first.matrix <- matrix(1:12, ncol=4)
first.matrix
second.matrix <- matrix(1:12, ncol=4, byrow=TRUE)
second.matrix
str(first.matrix)
str(second.matrix)
dim(first.matrix)
length(first.matrix)
# +vectors
vector1 <- c(12,4,5,6,9,3)
vector1
vector2 <- c(5,4,2,4,12,9)
vector2
matrix.one <- rbind(vector1,vector2) # each vector becomes a row in the matrix
matrix.one
cbind(1:3,4:6)
cbind(1:3,4:6,matrix(7:12,ncol=2)) # +matrix 
# indices
first.matrix
first.matrix[1:2,2:3]
first.matrix[2:3, ]
first.matrix[-2,-3]
#nr <- nrow(first.matrix) # num rows
#id <- nr * 2 + 2   # calculate the position of entry we want to exclude
#first.matrix[-id]   # note the 8 is missing from the results
#
# replacing values
first.matrix.orig <- first.matrix 
first.matrix
first.matrix[3,2] <- 4
first.matrix
first.matrix[2,] <- c(1,3)
first.matrix
first.matrix[1:2,3:4] <- c(8,4,2,1)
first.matrix
first.matrix <- first.matrix.orig
first.matrix
first.matrix + 4
first.matrix <- matrix(1:12, ncol=4)
first.matrix
second.matrix <- matrix(1:3, nrow=3, ncol=4)
second.matrix
first.matrix + second.matrix
# using this code: first.matrix <- matrix(1:12,ncol=4) \n first.matrix + 1:3 yields same result as above
first.matrix <- matrix(1:12, ncol=4)
first.matrix
first.matrix + 1:3
#
# Dataframes - stop here for next week...
# 
baskets.of.granny <- c(12,4,5,6,9,3)
baskets.of.gerry <- c(5,4,2,4,12,9)
baskets.of.team <- rbind(baskets.of.granny,baskets.of.gerry)
baskets.of.team
baskets.df <- as.data.frame(t(baskets.of.team))
baskets.df
str(baskets.df)
nrow(baskets.df)
length(baskets.df)
#
# combining data
employee <- c("John Doe", "Jim Doe", "Joe Doe")
salary <- c(21000,23500,150000)
startdate <- as.Date(c("2010-11-2","2008-3-25","2002-5-11"))

employee.data <- data.frame(employee,salary,startdate)
str(employee.data)
names(employee.data)
names(employee.data)[3] <- "first day"
employee.data
rownames(employee.data) <- c("Sous Chef","Chef","Executive Chef") 
employee.data
# dataset info
#
library(help="datasets")     
       
       
       
       
  ##################################################################################################################################
       
       #Individual Scripting Assignment

       #Create an R Markdown Word or HTML document which contains your information (name, etc), the question in Text form, 
       #and displays your code and the output for the below questions:

#Create a character vector with length of number-of-rows-of-iris-dataset (iris is a built-in dataset), such that, each element
#gets a character value – “greater than 5″ if the corresponding ‘Sepal.Length’ > 5, else it should get “lesser than 5″.


#1) Make the logic for above problem statement using a 'for-loop' and a 'if-else' statement

#2) Make the logic for above problem statement using a ifelse() function

#3) Create a logic for the same problem statement using apply() function


iris
head(iris)
names(iris)
char_vector <- c(iris$Sepal.Length)
char_vector



#1. Create a Vector which has a sequence of values of the Sepal Length
vec <- c(iris$Sepal.Length)
#2. Check
vec
length(vec)
print(vec[2])

# 3. Create a null vector called storage which is a character vector and has a length of 150
storage = character(150)
length(storage)
is.character(storage)


for (i in 1:150){
  if (vec[i]>5.0) {
    #print(i)
    storage[i] = 'greater than 5'
    #print("I'm here")
    }else{
      storage[i] ='lesser than 5' 
    }
  }
length(storage)
storage





char_vector <-c(iris$Sepal.Length)
ifelse(char_vector>5,print('greater than 5'),print('lesser than 5'))
length(char_vector)


















#Create a logic for the same problem statement using apply() function
