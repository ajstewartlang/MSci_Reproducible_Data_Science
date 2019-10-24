library(tidyverse) # Load the tidyverse packages
library(Hmisc) # Needed for correlation
library(car) # Needed for VIF calculations and DW test
library(MASS) # Needed for maths functions
library(broom) # Allows us to turn the output of models into dataframes
library(leaps) # Plots all-subsets regression models
library(olsrr) # Needed for ols regression
library(gvlma) # Easy test of linear model assumptions
library(ggcorrplot) # Needed to visualisation the correlation plot

my_data <- read_csv("data_files/Mult_regression.csv")

# Can house prices be predicted by (one or more of) Population, Crime (per 10000 people), 
# Average age, or Household income in a region?  We have data from 1,000 regions.
# First let's plot some individual graphs

ggplot(my_data, aes(x = House_price, y = Population)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Crime)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Average_age)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Household_income)) + 
  geom_point() + 
  geom_smooth(method = "lm")

rcorr(my_data$House_price, my_data$Population)
rcorr(my_data$House_price, my_data$Crime)
rcorr(my_data$House_price, my_data$Average_age)
rcorr(my_data$House_price, my_data$Household_income)

corr <- cor(dplyr::select(my_data, -Region, -X1))

ggcorrplot(corr , hc.order = TRUE, type = "lower",
           lab = TRUE)

# First let's build a null model
model0 <- lm(House_price ~ 1, data = my_data)

# First let's build a model with all predictors
model1 <- lm(House_price ~ Population + Crime + Average_age + Household_income, 
             data = my_data)

# Do we have any multi-colinearity issues?
vif(model1)

# Check to see if the full model is better than the null model
anova(model0, model1)

# Now get the summary of model1
summary(model1)

# Notice that Average_age and Household_income do not seem to predict house prices
# Let's drop them in model2
model2 <- lm(House_price ~ Population + Crime, data = my_data)

# Is model2 now better model1?
anova(model2, model1)

AIC(model1)
AIC(model2)

# Let's validate and look at some diagnostic plots
hist(residuals(model2))
qqnorm(residuals(model2))
qqline(residuals(model2))
plot(model2)

durbinWatsonTest(model2)

# Now let's do some stepwise regression to see what we end up with
steplimitsboth <- step(model0, scope = list (upper = model1), direction = "both")

summary(steplimitsboth)

pmodel <- ols_step_forward_p(model1)
pmodel

leapsmodels <- regsubsets (House_price ~ Population + Crime + Average_age + 
                             Household_income, data = my_data)
plot(leapsmodels, scale = "adjr2", main = "Models")


confint(steplimitsboth, level=0.95)

vif(steplimitsboth)


