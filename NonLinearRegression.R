################################################################################
# Multiple Regression, Non-Linear Regression, Over-fitting and Cross-Validation
################################################################################
board_games = read.csv("board_games_fall_2023.csv")
attach(board_games)
View(board_games)
install.packages("car")
library("car")
###############################################################
# Step 1
###############################################################
reg1=lm(avg_rating~year+avg_timeplay+weight)
residualPlot(reg1)
residualPlots(reg1)


###############################################################
# Step 2
###############################################################
reg2a = lm(avg_rating~avg_timeplay)
residualPlot(reg2a,quadratic = FALSE)
summary(reg2a)
ncvTest(reg2a)

require(lmtest)
install.packages("plm")
require(plm)
coeftest(reg2a,vcov = vcovHC(reg2a,type="HC1"))

###############################################################
# Step 3
###############################################################
#  A
reg3=lm(avg_rating~min_players+age+num_votes)
residualPlot(reg3)
residualPlots(reg3)
qqPlot(reg3, envelope=list(style="none"))
# Numerical approach: Bonferroni Outlier Test
outlierTest(reg3)

#  C
board_games2=board_games[-c(1197,3124),]
reg3C=lm(avg_rating~min_players+age+num_votes, data=board_games2)
summary(reg3)
summary(reg3C)


###############################################################
# Step 4
###############################################################
quantvars=board_games[, c(7, 8, 9,12)]
corr_matrix=cor(quantvars)
round(corr_matrix,3)
require(psych)
reg4b = lm(avg_rating~year+age+min_timeplay+max_timeplay)
vif(reg4b)
summary(reg4b)

reg4f = lm(avg_rating~year+age+max_timeplay)
summary(reg4f)

###############################################################
# Step 5
###############################################################
#  A:

### Step 1:
reg1=lm(avg_rating~avg_timeplay)  
reg2=lm(avg_rating~min_players)
reg3=lm(avg_rating~max_players) 
mreg=lm(avg_rating~avg_timeplay+min_players+max_players)

# Step 2:
install.packages("stargazer")
library(stargazer)
stargazer(reg1, reg2, reg3, mreg, type="html")

#  B:
summary(reg2)

#  C:
stargazer(reg1, reg2, reg3, mreg, type="html", 
  covariate.labels=c("Average timeplay", "Minimum number of players", 
  "Maximum number of players"), dep.var.labels = "Board game rating")

#  D:
stargazer(reg1, reg2, reg3, mreg, type="html", 
  covariate.labels=c("Average timeplay", "Minimum number of players", 
  "Maximum number of players"), dep.var.labels = "Board game rating", digits=2)

###############################################################
# Step 6
###############################################################
require(caTools)
require(splines)
require(methods)
library(car)
library(stargazer)

age1 = lm(avg_rating ~ age, data = board_games)
age2 = lm(avg_rating~poly(age,2),data = board_games)
age3 = lm(avg_rating~poly(age,3),data = board_games)
age4 = lm(avg_rating~poly(age,4),data = board_games)


# Rename the coefficients
names(age2$coefficients)=c("(Intercept)","age", "age2")
names(age3$coefficients)=c("(Intercept)","age", "age2","age3")
names(age4$coefficients)=c("(Intercept)","age", "age2","age3","age4")

stargazer(age1, age2, age3, age4, 
          type = "html",
          dep.var.labels = "Board game rating",
          covariate.labels = c("age","age<sup>2</sup>","age<sup>3</sup>","age<sup>4</sup>",
                               digits = 2,
                               align = TRUE,
                               column.labels = c("linear","Quadratic","Cubic","Qiartic")))

# B:

install.packages("ggpubr")

library(ggpubr)
library(ggplot2)
plot1=ggplot(board_games, aes(x = age, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Degree 1 Polynomial Fit", subtitle = "Scatterplot with Linear Fit")

plot2=ggplot(board_games, aes(x = age, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  labs(title = "Degree 2 Polynomial Fit", subtitle = "Scatterplot with Quadratic Fit")

plot3=ggplot(board_games, aes(x = age, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "blue") +
  labs(title = "Degree 3 Polynomial Fit", subtitle = "Scatterplot with Cubic Fit")

plot4=ggplot(board_games, aes(x = age, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = FALSE, color = "blue") +
  labs(title = "Degree 4 Polynomial Fit", subtitle = "Scatterplot with Quartic Fit")

install.packages("dplyr")
library(dplyr)

library(ggpubr)

plots_matrix=ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

plots_matrix


# C:
anova_result=anova(age1, age2, age3, age4)

# Print the ANOVA results
print(anova_result)

###############################################################
# Step 7
###############################################################
install.packages("stargazer")
library(stargazer)


#  A:
# rating = f(avg_timeplay)
# linear d=1
reg1=lm(avg_rating~poly(avg_timeplay, 1))
# quadratic d=2
reg2=lm(avg_rating~poly(avg_timeplay, 2))
# cubic d=3
reg3=lm(avg_rating~poly(avg_timeplay, 3))
# quadratic d=4
reg4=lm(avg_rating~poly(avg_timeplay, 4))

names(reg1$coefficients) = c("(Intercept)", "Average timeplay")
names(reg2$coefficients) = c("(Intercept)", "Average timeplay", "Average timeplay^2")
names(reg3$coefficients) = c("(Intercept)", "Average timeplay", "Average timeplay^2", "Average timeplay^3")
names(reg4$coefficients) = c("(Intercept)", "Average timeplay", "Average timeplay^2", "Average timeplay^3", "Average timeplay^4")

# Use stargazer
stargazer(reg1, reg2, reg3, reg4, type="html", align=TRUE,
          dep.var.labels = "Average rating", digit=2)


#  B:

library(ggplot2)
library(gridExtra)

# Create individual plots
plot1=ggplot(df, aes(x = avg_timeplay, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_line(aes(y = fit1), color = "green") +
  labs(title = "Degree 1", x = "Average timeplay", y = "Average rating") +
  theme_minimal()

plot2=ggplot(df, aes(x = avg_timeplay, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_line(aes(y = fit2), color = "green") +
  labs(title = "Degree 2", x = "Average timeplay", y = "Average rating") +
  theme_minimal()

plot3=ggplot(df, aes(x = avg_timeplay, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_line(aes(y = fit3), color = "green") +
  labs(title = "Degree 3", x = "Average timeplay", y = "Average rating") +
  theme_minimal()

plot4=ggplot(df, aes(x = avg_timeplay, y = avg_rating)) +
  geom_point(color = "grey") +
  geom_line(aes(y = fit4), color = "green") +
  labs(title = "Degree 4", x = "Average timeplay", y = "Average rating") +
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 1)


#  C:

anova(reg1, reg2, reg3, reg4) 


# ANOVA tests to compare models
anova_test_1_2=anova(reg1, reg2)
anova_test_2_3=anova(reg2, reg3)
anova_test_3_4=anova(reg3, reg4)

# Print the results
anova_test_1_2
anova_test_2_3
anova_test_3_4


###############################################################
# Step 8
###############################################################
reg8 = lm((avg_rating~poly(age,4)+avg_timeplay),data = board_games)
summary(reg8)
stargazer(reg8, 
          type="html",
          dep.var.labels=c("Board game rating"),
          covariate.labels=c("Average timeplay", "Age", "Age<sup>2</sup>", "Age<sup>3</sup>", "Age<sup>4</sup>", "Intercept"))


###############################################################
# Step 9
###############################################################

#  B:

library(splines)
install.packages("ggplot2")
library(ggplot2)
require(methods)
install.packages("ggpubr")
library("ggpubr")

plot=ggplot(board_games, aes(y=avg_rating, x=avg_timeplay))
scatter=geom_point(color="salmon")
plot+scatter

# Equally-spaced knots
k1=quantile(board_games$avg_timeplay, 0.25)
k2=quantile(board_games$avg_timeplay, 0.50)
k3=quantile(board_games$avg_timeplay, 0.75)

## polynomial splines regression models
poly_reg1 = lm(avg_rating~bs(avg_timeplay, knots=c(k1,k2,k3), degree = 1))
poly_reg2 = lm(avg_rating~bs(avg_timeplay, knots=c(k1,k2,k3), degree = 2))
poly_reg3 = lm(avg_rating~bs(avg_timeplay, knots=c(k1,k2,k3), degree = 3))

stargazer(poly_reg1,poly_reg2,poly_reg3,
          type ="html",
          title="Regression Results",
          align=TRUE,
          dep.var.labels=c("Board game rating"),
          no.space=TRUE)

#  C:

plot=ggplot(board_games, aes(y=avg_rating, x=avg_timeplay))
scatter= geom_point(col="grey")

spline_1 = geom_smooth(method="lm",formula=y~bs(x,knots=c(k1,k2,k3), degree=1), aes(col="red"))
dashed_sp1=geom_vline(xintercept=c(k1,k2,k3),linetype="dashed")
legend_sp1=scale_color_identity(name="3 knots",label="Linear",guide="legend")
i=plot+scatter+spline_1+dashed_sp1+legend_sp1

spline_2 = geom_smooth(method="lm",formula=y~bs(x,knots=c(k1,k2,k3), degree=2), aes(col="red"))
dashed_sp2=geom_vline(xintercept=c(k1,k2,k3),linetype="dashed")
legend_sp2=scale_color_identity(name="3 knots",label="Linear",guide="legend")
j=plot+scatter+spline_2+dashed_sp2+legend_sp2

spline_3 = geom_smooth(method="lm",formula=y~bs(x,knots=c(k1,k2,k3), degree=3), aes(col="red"))
dashed_sp3=geom_vline(xintercept=c(k1,k2,k3),linetype="dashed")
legend_sp3=scale_color_identity(name="3 knots",label="Linear",guide="legend")
k=plot+scatter+spline_3+dashed_sp3+legend_sp3

ggarrange(i,j,k, ncol = 1, nrow = 3)


###############################################################
# Step 10
###############################################################
library(caTools)

#for linear model: 
mse_process = 0
for (i in 0:30){
  sample = sample.split(board_games$avg_rating, SplitRatio=0.5)
  train_set=subset(board_games, sample==TRUE)
  test_set=subset(board_games, sample==FALSE)
  # fit the first model: 
  fit=lm(avg_rating~weight, data=train_set)
  
  actual=test_set$avg_rating
  prediction=predict(fit, test_set)
  squared_error=(actual-prediction)^2
  mse=mean(squared_error)
  mse_process=mse_process + mse
}
mse_result = round(mse_process/30,3)
mse_result
# for poly model: 
dimen = c(2,3,4,5,6,7,8,9,10)
for (i in dimen){
  mse_process = 0
  for (j in 0:30){
    sample = sample.split(board_games$avg_rating, SplitRatio=0.5)
    train_set=subset(board_games, sample==TRUE)
    test_set=subset(board_games, sample==FALSE)
    # fit the model: 
    #print(i)
    fit=lm(avg_rating~poly(weight,i), data=train_set)
    actual=test_set$avg_rating
    prediction=predict(fit, test_set)
    squared_error=(actual-prediction)^2
    mse=mean(squared_error)
    mse_process=mse_process + mse
    #print(j)
  }
  #print(mse_process)
  mse_result = round(mse_process/30,3)
  print(paste(i,"final result",mse_result))
}

###############################################################
# Step 11
###############################################################

# C:

library(boot) 
### Linear, d=1
fit=glm(avg_rating~weight, data=board_games)
mse=cv.glm(board_games, fit)$delta[1]
mse

#  D:
mse=rep(NA,10)
for (i in 1:10) {
  fit=glm(avg_rating~poly(weight,i), data=board_games)
  mse[i]=cv.glm(board_games, fit)$delta[1]
}
mse

#  E:
plot(mse)
lines(mse, col="red")
which.min(mse)          
min(mse)           

###############################################################
# Step 12
###############################################################
library(boot)
#linear
fit=glm(avg_rating~num_votes, data=board_games)
mse=cv.glm(board_games, fit, K=5)$delta[1]
round(mse,3)
#poly d=2:10
for (i in(2:10)){
  fit=glm(avg_rating~poly(num_votes,i), data=board_games)
  mse=cv.glm(board_games, fit, K=5)$delta[1]
  print(paste(i,"the result is",round(mse,3)))
}
###############################################################
# Step 13
###############################################################

#  A:
library(boot)
library(splines)

# Define knots for each predictor
k_age=quantile(board_games$age, c(0.25, 0.50, 0.75))
k_year=quantile(board_games$year, c(0.25, 0.50, 0.75))
k_num_votes=quantile(board_games$num_votes, c(0.25, 0.50, 0.75))
k_avg_timeplay=quantile(board_games$avg_timeplay, c(0.25, 0.50, 0.75))

# Initialize the minimum MSE to a large value
min_mse=Inf

# Store the optimal combination
optimal_combination=c(NA, NA, NA, NA)

# Run k-fold cross-validation for each combination
for (a in 1:5) {
  for (b in 1:5) {
    for (c in 1:5) {
      for (d in 1:5) {
        model=glm(avg_rating ~ bs(age, degree=a, knots=k_age) +
                       bs(year, degree=b, knots=k_year) +
                       bs(num_votes, degree=c, knots=k_num_votes) +
                       bs(avg_timeplay, degree=d, knots=k_avg_timeplay),
                     data=board_games)
        
        # Compute MSE for this model using 20-fold cross-validation
        cv_results=cv.glm(board_games, model, K=20)
        mse=cv_results$delta[1] 
        
        # Update the minimum MSE and optimal combination if needed
        if (mse < min_mse) {
          min_mse=mse
          optimal_combination=c(a, b, c, d)
        }
      }
    }
  }
}

print(optimal_combination)
print(min_mse)
