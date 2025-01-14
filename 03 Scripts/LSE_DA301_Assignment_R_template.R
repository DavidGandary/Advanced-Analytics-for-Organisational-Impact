# Can loyalty points be predicted based on demographic or behavioral factors?

## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

# Your code here.
# Determine your working directory
getwd()

# Change your current directory.
setwd(dir= "/Users/davidgandary/Documents/Data Analytics Course Accelerator/LSE DA Course 3 files/Assignment/LSE_DA301_assignment_files_new")

# Import the tidyverse library.
library(tidyverse)

# Import the clean turtle_review.csv data file into R
turtle_reviews <- read.csv(file = 'turtle_reviews_clean.csv', header = TRUE)

# View the first six lines of the data frame.
head(turtle_reviews)

# Sense-check the data set
# Return the structure of the data frame.
str(turtle_reviews)

# Check the type of the data frame.
typeof(turtle_reviews)

# Check the class of the data frame.
class(turtle_reviews)

# Check the dimensions of the data frame
dim(turtle_reviews)

# Delete columns reviews and summary.
turtle_reviews <- subset(turtle_reviews,
                     select = -c(review, summary))


# View the column names.
names(turtle_reviews)

#Import the ggplot2 libaray.
library(ggplot2)

#Compare age and spending score based on education level.
ggplot(turtle_reviews,
       mapping=aes(x=age, y=spending_score)) +
  geom_point(color='blue',
             alpha=0.75,
             size=2.5) +
  scale_x_continuous(breaks=seq(0, 70, 5), "Age of the Customer") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Spending score") +
  labs(title="Relationship between age and spending score",
       subtitle="Based on education level") + 
  facet_wrap(~education)
#Output shows scatter plot separating each individual education group, 
#helping to understand spread of data in more context.

#Explore how education level correlates with income
ggplot(turtle_reviews, aes(x=education, y=remuneration)) +
  geom_boxplot() + labs(title="Relationship between education and income")

#Output shows boxplot to provide a clear visual summary of the central tendency 
#and spread of the data, for each education level based on income to 
# see if higher education levels correspond to higher incomes.
#Interesting insight that basic education level holds the highest median 
#remunerartion which shows a more concentrated distribution of data.
#statistical analysis (e.g., hypothesis testing) could be leveraged to determine 
#if these differences are statistically significant.

## Scatterplot with a line-of-best-fit from linear regression model.
ggplot(turtle_reviews,
       mapping=aes(x=spending_score, y=loyalty_points)) +
  geom_point() +
  geom_smooth(method=lm)
#Output shows simple regression model to see relationship between loyalty points
#and spending score. 
#Insight can be shown into how many customers who spend more don't follow line of 
#best fit and therefore could potentially be receiving more loyalty points than 
#they should. Linear regression model created in python shows similar model 
#confirming reliability could be tested and reviewed to confirm this.

#Histogram to show mean loyalty points based on age groups
age_breaks <- seq(15, 75, by = 10)  # Define breaks in increments of 5 from 17 to 72
age_labels <- paste(age_breaks[-length(age_breaks)], "-", age_breaks[-1])  # Create labels for age ranges

# Group ages into specified ranges using cut()
turtle_reviews$age_group <- cut(turtle_reviews$age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)
loyalty_summary <- aggregate(loyalty_points ~ age_group, data = turtle_reviews, FUN = mean)
# Plot histogram using ggplot2
ggplot(loyalty_summary, aes(x = age_group, y = loyalty_points)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Age Groups", y = "Mean Loyalty Points", title = "Mean Loyalty Points by Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# The grouped ages histogram is displayed to show the average loyalty points
#between different age groups.
#Insight that 25-35 year olds have the highest average mean loyalty points and
#15-25 year olds have the lowest.





###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Your code here.
# View turtles_review_clean dataframe
head(turtle_reviews)

# Compute descriptive statistics.

# Determine descriptive statistics of the data set.
summary(turtle_reviews)
summary(turtle_reviews$loyalty_points)

# Measure central tendencies of loyalty with mean and median.
mean(turtle_reviews$loyalty_points)
median(turtle_reviews$loyalty_points)


# Statistics of extreme values (max and min).
min (turtle_reviews$loyalty_points)
max (turtle_reviews$loyalty_points)


# Measure the variability of loyalty points values.
# Range = Maximum - Minimum.
max(turtle_reviews$loyalty_points)- min(turtle_reviews$loyalty_points)


# Function to calculate Q1.
quantile(turtle_reviews$loyalty_points, 0.25)  

# Function to calculate Q2.
quantile(turtle_reviews$loyalty_points, 0.75)   

# Function to calculate IQR.
IQR(turtle_reviews$loyalty_points)    


# Function to determine the variance.
var(turtle_reviews$loyalty_points)


# Function to return the standard deviation.
sd(turtle_reviews$loyalty_points)

# Measure normality in loyalty points.
# Q-Q plot:
qqnorm(turtle_reviews$loyalty_points)   
# Add a reference line:
qqline(turtle_reviews$loyalty_points, col='red')
#We are comparing the shape of the data with the shape of the normal distribution
# The points closest to the normal distribution line are 
#those that are of the mean of the normal.
#Points furthest away from straight line are between 1 and 3 
#In more extreme cases values should be higher than they are.
#Customers with loyalty points 
#in tails are higher than we would expect against normal distribution
#Indicating that distribution may have heavier tails than we would expect.


# Shapiro-Wilk test:
shapiro.test((turtle_reviews$loyalty_points))
# Our p-value is <0.05,so this means we can reject the null-hypothesis
# Important to note that these tests are sensitive to sample size.


# Check for skewness.
# Install the moments package and load the library.
install.packages('moments') 
library(moments)


# Now we can check for skewness.
skewness(turtle_reviews$loyalty_points)
# Our output suggests a positive skewness.


#Check for kurtosis.
kurtosis(turtle_reviews$loyalty_points)
# Our kurtosis value is more than 3, 
#Suggesting our data is heavy tail distribution

#Conclusion of the review of how customers accumulate points
#The data set contains 2000 rows. The loyalty points column is made up of
#a score based on the point value of the purchase,converting the monetary value 
#to point value,and the point value of an action (purchase). Therefore based on 
# the average loyalty points accumulated per customer is 1578 points. 

#The descriptive statistics indicated that the data might be normally distributed
#heavy tail.

#The heavy tails indicate that the extreme number of loyalty points 
#are more extreme than expected. Therefore we can conclude that Turtle may not 
#implement an accurate system that gives customers more loyalty points than they
#should be receiving.


# Create a multiple linear regression model using selected (numeric) features.
model2 <- lm(loyalty_points ~ age + remuneration + spending_score, data = turtle_reviews)

# View the summary stats.
summary(model2)

#Evaluate the goodness of fit and interpret summary stats
#R-squared measures the proportion of variance in the dependent variable (loyalty points)
#that is explained by the independent variables (selected features) in the model. 
#A higher R-squared value indicates a better fit. 0.8399 therefore indicates a good fit.

#Adjusted R-squared is a modified version of R-squared that adjusts for the 
#number of predictors in the model. It penalizes the addition of unnecessary 
#variables and is useful for comparing models with different numbers of predictors.
#Therefore this value = 0.8397 is more suitable for evaluating goodness of fit of this model 
#and still indicates a good fit proving great confidence in model.

# Create a visualisation to determine normality of data set.
qqnorm(residuals(model2))
qqline(residuals(model2), col='red')

#This plot assesses model assumptions such as normality of residuals, linearity, and homoscedasticity.
#Majority of the point lie close to the normal distribution line.
#We can suggest that model is normally distributed

#Demonstrate how the model could be used to predict given specific scenarios
new_data <- data.frame(age = 30, remuneration = 55000, spending_score = 55)  # new observation
new_prediction <- predict(model2, newdata = new_data)
print(new_prediction)
#Output displays index of 1 and a predicted value of 1870470 loyalty points.

###############################################################################
###############################################################################




