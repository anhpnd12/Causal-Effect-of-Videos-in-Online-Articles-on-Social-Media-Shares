# Workspace preliminaries
setwd("**fill in with path**") # set working directory
rm(list = ls()) # clear workspace

# Load data
data <- read.csv("D5.2 Mashable.csv")




########### Task 1
# Convert num_videos to numeric and create treatment variable
data$num_videos <- as.numeric(as.character(data$num_videos))
data$treatment <- ifelse(data$num_videos > 0, 1, 0)

# Calculate Pearson's correlation coefficient
library("ggpubr")
library("ggplot2")

ggscatter(data, x = "treatment", y = "shares", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "treatment", ylab = "shares")

res <- cor.test(data$treatment, data$shares, 
                method = "pearson")
res
## p-value < 0.05, indicate that there is correlation between article with videos and number of shares


# Fit a linear regression model
model <- lm(shares ~ treatment, data = data)

# Print the model summary
summary(model)

## p-value < 0.05, indicate that including at least one video is associated with a typically larger number of shares
# The coefficient estimate = 1418.71, positive and statistically significant, indicate a typically larger number of shares for articles with videos






########## task 2
# Check summary statistics
summary(data)
table(data$num_videos) # check number of videos

library(tableone)
library(MatchIt)

# Convert categorical variables to factors
data$category <- as.factor(data$category)
data$weekday <- as.factor(data$weekday)

# Check balance of covariates between treated and untreated groups
print(CreateTableOne(vars = c("num_imgs", "num_keywords", "category", "weekday", "shares"), data = data, strata = "treatment"), smd = TRUE)

# Checking overlap
# Fit logistic regression model to calculate propensity scores
data$pscore <- predict(glm(treatment ~ num_imgs + num_keywords + category + weekday + shares, data = data, family = "binomial"), type = "response")
# Plot histograms of propensity scores for treated and untreated groups
histogram(~ pscore | treatment, data = data)
#### The majority of observations have a pscore between 0.1 and 0.6 and distributions are overall similar. This suggests that the covariate balance between the groups is satisfactory.
#### The distributions don’t overlap in the range of pscore above 0.6. This suggests that there may be some covariate imbalance in this range, which could affect the reliability of the treatment effect estimate.

# Perform matching
matched <- matchit(treatment ~ num_imgs + num_keywords + category + weekday + shares, method = "nearest", data = data)
# Create matched data set 
data_matched = match.data(matched)
data_matched = data_matched[data_matched$pscore<=0.6,] # focus on area with enough overlap
dim(data_matched)

# Check balance of covariates between matched treated and untreated groups
print(CreateTableOne(vars = c("num_imgs", "num_keywords", "category", "weekday", "shares"), data = data_matched, strata = "treatment"), smd = TRUE)

### The untreated sample contains about 25,000 observations while the treated one only around 13,700 observations. For the matched sample, the treated and untreated samples are the same size (13,686 observations) and the sample is well balanced. This result suggests that matching procedure was successful at recreating the parallel worlds situation and we can treat our data largely as if it were from a randomize controlled experiments 
### SMDs for covariates are below the conventional threshold of 0.1, indicating that there are no substantial differences between the two groups in terms of the covariates included in the matching procedure








############ task 3
### Estimate ATE
summary(lm(shares ~ treatment, data = data_matched))
summary(lm(shares ~ treatment, data = data)) # compare to estimate from the unmatched sample

### The estimated ATE of treatment (videos) on shares:
# Matched data: ATE = 1005.6, SE = 165.6, p-value < 0.01, statistically significant. Having videos tends to be associated with about 1005.6 point increase (30.4% increase) in the number of shares in matched scenario
# Unmatched data: ATE = 1418.71, SE = 132.76, p-value < 0.01, statistically significant. Having videos tends to be associated with about 1418.71 point increase (49% increase) in the number of shares in unmatched scenario

# After matching on the propensity score, the estimated effect size of the video treatment on the number of shares in matched is smaller than in the unmatched scenario, suggesting that the matching procedure has reduced the bias in the estimation of the treatment effect. 









########### task 4
# Select a subsample of the full dataset
set.seed(123)
data_subsample <- data[sample(nrow(data), 10000), ]

# Create treatment variable, articles included > 1 video = 1, < 1 video = 0 
data_subsample$treatment1 <- ifelse(data_subsample$num_videos > 1, 1, 0)

# Convert categorical variables to factors
data_subsample$category <- as.factor(data_subsample$category)
data_subsample$weekday <- as.factor(data_subsample$weekday)

# Check balance of covariates between treated and untreated groups
print(CreateTableOne(vars = c("num_imgs", "num_keywords", "category", "weekday", "shares"), data = data_subsample, strata = "treatment"), smd = TRUE)

# Checking overlap
# Fit logistic regression model to calculate propensity scores
data_subsample$pscore <- predict(glm(treatment1 ~ num_imgs + num_keywords + category + weekday + shares, data = data_subsample, family = "binomial"), type = "response")
# Plot histograms of propensity scores for treated and untreated groups
histogram(~ pscore | treatment1, data = data_subsample)
#### The majority of observations have a pscore between 0.1 and 0.6 and distributions are overall similar. This suggests that the covariate balance between the groups is satisfactory.
#### The distributions don’t overlap in the range of pscore above 0.6. This suggests that there may be some covariate imbalance in this range, which could affect the reliability of the treatment effect estimate.


# Perform matching
matched1 <- matchit(treatment1 ~ pscore, method = "nearest", data = data_subsample)
# Create matched data set 
data_matched1 = match.data(matched1)
data_matched1 = data_matched1[data_matched1$pscore<=0.6,] # focus on area with enough overlap
dim(data_matched1)

# Check balance of covariates between matched treated and untreated groups
print(CreateTableOne(vars = c("num_imgs", "num_keywords", "category", "weekday", "shares"), data = data_matched1, strata = "treatment"), smd = TRUE)
### SMD values for num_imgs, num_keywords, category, weekday, and shares are below the recommended threshold of 0.1, indicating good balance

# Estimate treatment effect
summary(lm(shares ~ treatment1, data = matched_data1))

##### p-value > 0.05, indicate that there is no evidence to suggest that including more than 1 video in an article has a causal effect on the number of shares
##### conclude: The difference in the new treatment can lead to different effects. It's possible that the presence of any video in an article has a positive effect on the number of shares (task 1-3), but having more than one video (task 4) doesn't have an additional effect on shares. Additionally, the size of the subsample is much smaller than the full dataset, which may lead to less statistical power and higher uncertainty in the treatment effect estimate.
