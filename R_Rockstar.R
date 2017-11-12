

## arbuthnot data prctice

source("http://www.openintro.org/stat/data/arbuthnot.R")

arbuthnot  #This is a dataframe. A data frame is analogous to a table in SQL or speadsheet with rows and columns

dim(arbuthnot) #How many rows and columns does the data frame has?

names(arbuthnot) # What are the column headers aka field names in the data frame

arbuthnot$boys # I want to see the the data frame where I see all values of # boys that were baptized. This is a vector which has a set of values, 
# They are not structured in a table

arbuthnot$girls # I want to see the the data frame where I see all values of # girls that were baptized. This is a vector which has a set
# of values They are not structured in a table

plot(x = arbuthnot$year, y = arbuthnot$boys) # Plot a Scatter Plot of the boys with X-axis being Year and Y axis being COunt of boys

plot(x = arbuthnot$year, y = arbuthnot$girls, type = 'l') # Plot a Line Chart to show the trend of #girls baptized by year

# ?plot  If you need to seek help there should be a question in front of the function
5218 + 4683  # R acts as a calculator

arbuthnot$boys + arbuthnot$girls # Add the vectors simultaneously by just adding the vectors. The new vector represents sum of the vectors 

plot(x = arbuthnot$year,y = arbuthnot$boys+arbuthnot$girls, type = 'l')  # How many girls & boys were baptized per year?

plot(arbuthnot$year, arbuthnot$boys+arbuthnot$girls, type = 'l')  # Did not mention x and y as the default 1st variable is year and default 2nd variable is y

5218/4683  # Ratio of the number of boys to number of girls baptized in 1629

arbuthnot$boys/arbuthnot$girls  # Act on the complete vector. WHat is the ratio of # boys to girls for all years 

5218/(5218 + 4683)  # What is the proportion of new borns that are boys

arbuthnot$boys/(arbuthnot$boys + arbuthnot$girls)  # Wat is the proportions of new borns that are boys for all the years

plot(x = arbuthnot$year,y = arbuthnot$boys/(arbuthnot$boys+arbuthnot$girls), type = 'l') # What is the proportion of new borns that are boys in a line chart

arbuthnot$boys>arbuthnot$girls  # Creating logical data where values are TRUE or FALSE. Comparing 2 numerical values




## present dataset is the # new borns in the US by gender

source("http://www.openintro.org/stat/data/present.R") # Read the dataset on the web in R

present  # Print the Data Frame

dim(present) # What are the dimensions in the R data frame

names(present) # What are the fields in the R data frame

present$boys/(present$boys + present$girls) # Create a vector to find the propotion of new borns that are boys 

plot(x= present$year, y = present$boys/present$girls, type = 'l') # plot a line chart of ratio of boys to girls. Whats the insight?

plot(x= present$year, y = present$girls/present$boys, type = 'l') # plot a line chart of ratio of girls to boys Whats the insight?

plot(present$year, present$girls + present$boys, type = 'l') # Plot a line chart of the totla number of new borns in the US by year





# cdc.R Dataset
source("http://www.openintro.org/stat/data/cdc.R")  # Load the dataset into the R Workspace

names(cdc) # What are the names of the variables in the dataset?
#This returns the names genhlth, exerany, hlthplan, smoke100, height, weight, wtdesire, age, and gender. Each one of these variables corresponds to a question that was asked in the survey. For example, for genhlth, respondents were asked to evaluate their general health, responding either excellent, very good, good, fair or poor. The exerany variable indicates whether the respondent exercised in the past month (1) or did not (0). Likewise, hlthplan indicates whether the respondent had some form of health coverage (1) or did not (0). The smoke100 variable indicates whether the respondent had smoked at least 100 cigarettes in her lifetime. The other variables record the respondent’s height in inches,  weight in pounds as well as their desired weight, wtdesire, age in years, and gender.

head(cdc) #  a look at the first few entries (rows) of the cdc.R data

tail(cdc) # a look at the last few entries (rows) of the cdc.R data

summary(cdc) #A good first step in any analysis is to distill all of that information into a few summary statistics and graphics. As a simple example, the function summary returns a numerical summary: minimum, first quartile, median, mean, second quartile, and maximum.
# R also has built-in functions to compute summary statistics one by one. For instance, to calculate the mean, median, and variance of weight

mean(cdc$weight)

median(cdc$weight)

var(cdc$weight)

#consider the sample frequency or relative frequency distribution. The function table does this for you by counting the number of times each kind of response was given. For example, to see the number of people who have smoked 100 cigarettes in their lifetime

table(cdc$smoke100)  # Groups the categorical variable by levels and applies a summary function of count. similar to select smoke100,count(*) from cdc.R group by 1

#  look at the relative frequency distribution by typing

table(cdc$smoke100)/20000  # 20000 is the relative frequency distribution

barplot(table(cdc$smoke100))  #make a bar plot of the entries in the table by putting the table inside the barplot command. This is a nested R statement which is an important concept.

smoke <-table(cdc$smoke100)  # The special symbol <- performs an assignment, taking the output of one line of code and saving it into an object in your workspace.

barplot(smoke)

# Create a numerical summary for height and age, and compute the interquartile range for each. Compute the relative frequency distribution for gender and exerany. How many males are in the sample? What proportion of the sample reports being in excellent health?

summary(cdc$height) # Interquartile ranges for height

summary(cdc$age) # Interquartile ranges for age

table(cdc$gender) # Count of males anf females in the population

table(cdc$exerany)/20000   # Relative Frequency Distribution

table(cdc$gender)/20000 #  Relative Frequency Distribution

# The table command can be used to tabulate any number of variables that you provide. For example, to examine which participants have smoked across each gender, we could use the following.

table(cdc$gender, cdc$smoke100)

# Different Variants are below
table(cdc$genhlth, cdc$smoke100)

table(cdc$gender, cdc$genhlth, cdc$smoke100)

table(cdc$genhlth, cdc$gender,cdc$smoke100)


#To create a mosaic plot of this table

mosaicplot(table(cdc$gender,cdc$smoke100))

# Size of the Data Frame
dim(cdc)


#  to see the sixth variable of the 567th respondent
cdc[567,6]

names(cdc) # What are the column headers?

cdc[1:10,6] # What are the weights of the 1st 10 respondents

# In this expression, we have asked just for rows in the range 1 through 10. R uses the : to create a range of values, so 1:10 expands to 1, 2, 3, 4, 5, 6, 7, 8, 9, 10. You can see this by entering following
1:10

# if we want all of the data for the first 10 respondents, type
cdc[1:10,]


# By leaving out an index or a range (we didn’t type anything between the comma and the square bracket), we get all the columns. When starting out in R, this is a bit counterintuitive. As a rule, we omit the column number to see all columns in a data frame. Similarly, if we leave out an index or range for the rows, we would access all the observations, not just the 567th, or rows 1 through 10. Try the following to see the weights for all 20,000 respondents fly by on your screen

cdc[,6] # GIves all the weights in the dataset

# ALternative method to get all the values of weight:

cdc$weight


# Give the 567th respondent's weight

cdc$weight[567]

# Give the 1st 10 weights of the respondents

cdc$weight[1:10]

# Subsetting based on a specific condition
# commands produce a series of TRUE and FALSE values. There is one value for each respondent, where TRUE indicates that the person was male (via the first command) or older than 30 (second command).
cdc$gender == "m"

cdc$age > 30  # All respondents having age greater than 30

mdata <- subset(cdc, cdc$gender == "m")  # All respondents who are males

head(mdata)

m_and_over30 <- subset(cdc, gender == "m" & age > 30) # All respondents who are males and greater than 30 years of age

m_and_over30 

m_or_over30 <- subset(cdc, gender == "m" | age > 30) #  All respondents who are males OR greater than 30 years of age

m_or_over30

# Create a new object called under23_and_smoke that contains all observations of respondents under the age of 23 that have smoked 100 cigarettes in their lifetime. 

under23_and_smoke <- subset(cdc, cdc$age<23 & cdc$smoke100 == 1)

under23_and_smoke


# Two common ways to visualize quantitative data are with box plots and histograms. We can construct a box plot for a single variable with the following command

boxplot(cdc$height)

summary(cdc$height) # compare the locations of the components of the box by examining the summary statistics.

boxplot(cdc$height ~ cdc$gender) # The purpose of a boxplot is to provide a thumbnail sketch of a variable for the purpose of comparing across several categories. So we can, for example, compare the heights of men and women using the ~ sign in the boxplot function


# Formula for BMI is BMI=weight (lb)height (in)2∗703
# Lets Calculate the BMI using the height and weight fields in the cdc dataset
# new object called bmi and then creates box plots of these values, defining groups by the variable  cdc$genhlth.
bmi <- (cdc$weight / cdc$height^2) * 703

bmi

boxplot(bmi ~ cdc$genhlth) 

# We can look at the histogram for the age of our respondents with the following command

hist(cdc$age)

# Histograms are generally a very good way to see the shape of a single distribution, but that shape can change depending on how the data is split between the different bins. You can control the number of bins by adding an argument to the command. In the next two lines, we first make a default histogram of bmi and then one with 50 breaks.

hist(bmi)

hist(bmi, breaks = 50)






