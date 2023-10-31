# Board Game Rating Regression Analysis

This R code focuses on **regression analyses** to study the relationship between board game ratings (`avg_rating`) and several potential predictors such as year of release, time of play, weight, age, and number of votes, among others. I will break down the code for you.

## Step 1: Multiple Linear Regression
A linear regression model `reg1` is created with `avg_rating` as the dependent variable and year, `avg_timeplay`, and weight as independent variables. Residual plots are then generated to visually inspect the residuals (errors).

## Step 2: Diagnosing Heteroscedasticity
This step focuses on diagnosing the potential heteroscedasticity in the residuals (i.e., if the spread or variance of residuals changes across different levels of the independent variable). A new regression model `reg2a` is created with `avg_rating` predicted by `avg_timeplay`.

## Step 3: Outlier Detection and Handling
A regression model `reg3` is constructed with `avg_rating` being predicted by `min_players`, age, and `num_votes`. After plotting residuals, outlier detection is applied and two potential outliers are removed. Then, a new regression model `reg3C` is constructed without these outliers, and its summary is compared with that of `reg3`.

## Step 4: Checking Multicollinearity 
Correlations among several quantitative variables are computed. Another regression model `reg4b` is created and checked for multicollinearity using the variance inflation factor (VIF). Based on the VIF, `min_timeplay` is removed and a new regression model `reg4f` is created.

## Step 5: Presenting Results
This step mainly focuses on presenting results using the `stargazer` package, which provides a nice table of regression outputs. The regression models presented compare `avg_rating` against `avg_timeplay`, `min_players`, and `max_players`.

## Step 6: Polynomial Regression on Age
Polynomial regression is employed here, examining how the age of the board game (in years) might predict its rating. Four models of increasing polynomial degree (from linear to quartic) are created and summarized in a table.

## Step 7: Polynomial Regression on avg_timeplay
This step examines how the average time of play (`avg_timeplay`) predicts the rating using polynomial regression. Four models are created, just as in Step 6.

## Step 8: Combined Polynomial Regression
A regression model `reg8` is created with a fourth-degree polynomial term of age and `avg_timeplay` as predictors.

## Step 9: Splines Regression 
Splines regression is utilized to provide a flexible way of modeling the relationship between `avg_rating` and `avg_timeplay`. The `bs` function is used to generate the spline basis functions, and three models of increasing degree are created. Knots for the splines are based on quantiles of `avg_timeplay`.

## Step 10: Simulation-based Evaluation
A simulation-based approach is taken here to estimate the mean squared error (MSE) for the linear regression model that predicts `avg_rating` using the weight of the board game. The code iterates through the process of splitting the data into training and testing sets, fitting a linear regression model on the training set, and evaluating its MSE on the testing set. The average MSE across the 30 iterations is then computed. The same process is then applied to polynomial models of increasing degree, but this part of the code is truncated.

---

**Overall**, this R code conducts comprehensive regression analyses examining the relationship between board game ratings and several potential predictors. It uses both linear regression and more advanced methods like polynomial regression and splines regression. The code also incorporates simulation-based techniques and visualization to assess model performance and diagnose potential issues.

[![MIT license](https://img.shields.io/badge/License-MIT-blue.svg)](https://lbesson.mit-license.org/)


