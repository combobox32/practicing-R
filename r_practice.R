library(readxl)
library(dplyr)
library(ggplot2)

# Read dataset


# Descriptive statistics

summary(dataset)


# Histograms for my variables


# Sentiment

ggplot(data = dataset , 
       aes(x=Sentiment)) + 
  geom_histogram(color="blue", fill="white", bins = 10) 

# Ratings

ggplot(data = dataset , 
       aes(x=Ratings)) + 
  geom_histogram(color="white", fill="green", bins = 10) 

# Budget

ggplot(data = dataset , 
       aes(x=Budget)) + 
  geom_histogram(color="blue", fill="white", bins = 10) 

# Screens

ggplot(data = dataset , 
       aes(x=Screens)) + 
  geom_histogram(color="blue", fill="white", bins = 10) 

# Views

ggplot(data = dataset , 
       aes(x=Views)) + 
  geom_histogram(color="blue", fill="white", bins = 10) 
 
# Likes

ggplot(data = dataset , 
       aes(x=Likes)) + 
  geom_histogram(color="blue", fill="white", bins = 10) 

# Dislakes

ggplot(data = dataset , 
       aes(x=Dislikes)) + 
  geom_histogram(color="blue", fill="white", bins = 10) 

# Comments

ggplot(data = dataset , 
       aes(x=Comments)) + 
  geom_histogram(color="blue", fill="white", bins = 10) 


# {dplyr} - practice
 
# Example of using arrange()

dataset %>%
  select(Ratings, Comments) %>%
  arrange(Comments) %>%
  head()

# Display first 20 observations of variables Movie, Year, Genre

result <- select(dataset, 
                    Movie, 
                    Year, 
                    Genre)
head(result, 20)

# Group by Genre

resultsum <- summarise(group_by(dataset, 
                                Genre), 
                       divSum = sum(Ratings)
)
resultsum

# Scatter plot  N01
# Prediction of relationship between Likes and Comments for some movies

# There is possitive correlation between variables Likes i Comments
# Conclusion:
# When number of Likes decreases, number of Comments decreases too

library(car)

scatterplot(Likes ~ Comments | Year, data=dataset)

# Scatter plot  N02 : Values of variables Ratings i Comments

# Independent variable - Comments
# Dependent variable - Rattings

# Data don't form a line or a curve => The variables aren't correlated

ggplot(data = dataset, mapping = aes(x = Ratings, y = Comments)) + 
  geom_point(alpha = 0.5, aes(color = Ratings))

# Lollipop Chart 

ggplot(data = dataset) + 
  stat_summary(
    mapping = aes(x = Ratings, y = Comments),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# Linear Regression

# Y = 1460.93 X + 56.63

varlm=lm(formula = Comments~Ratings, data = dataset)

varlm

# (H0): beta coefficients associated with the variables 
# are equal to zero.

# (H1): beta coefficients associated with the variables 
# are not equal to zero.

# p-value: 0.8126 => 
# (H0) coefficient ?? of the predictor is zero
# My model is not statistically significant

# t values: 0.939 and 0.237

# Column Pr(>|t|) probability to get a t-value 
# as high or higher than the observed value
# In my dataset for the first variable I get: 0.348
# And for the second variable I get: 0.813
# Conclusion: The coefficients are not significant

# R-squared: 0.0002458 
# and adjusted R² = -0.00412
# Higher the better => 
# My values are low, this is not good

summary(varlm)

# 95% confidence intervals

confint(varlm)


scatter.smooth(x=dataset$Ratings, y=dataset$Comments, 
               main="Comments ~ Ratings")


# Residuals

plot(varlm$residuals,
     pch = 1, col = "blue")

# Add regression line

abline(varlm) 

# Linear regression with a quadratic coefficient

lmpom2 = lm(Comments~Ratings + I(Ratings^2), data = dataset) 

# The results

summary(lmpom2) 

# Let's see what happened with residuals => Improved model (not significantly)

plot(lmpom2$residuals,
     pch = 9, col = "blue")
