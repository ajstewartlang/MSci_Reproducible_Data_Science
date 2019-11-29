library(lme4) # needed for linear mixed models
library(lmerTest) # gives us p-values for parameters in linear mixed models 
library(emmeans) # allows us to run pairwise comparisons/contrsasts
library(tidyverse) # load the tidyverse packages
library(ordinal) # needed for CLMM - cumulative link mixed models for ordinal data

# Linear model
genderheightdata <- read_csv("data_files/genderheightdata.csv")

genderheightdata$gender <- as.factor(genderheightdata$gender)

ourmodel <- lm(height ~ gender, data = genderheightdata)
summary(ourmodel)

ggplot(genderheightdata, aes(x = gender, y = height, group = 1)) + 
  geom_point() +
  geom_smooth(method = "lm")

ageheightdata <- read_csv("data_files/ageheightdata.csv")

ourmodel <- lm(height ~ age, data = ageheightdata)
summary(ourmodel)

ggplot(ageheightdata, aes(x = age, y = height)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Start of random slopes and intercepts simulation ####
# You really don't need to understand the following to follow the slides
# The code below just creates the simulated data set
subject <- rep(1:10, each = 10)
condition <- rep(c("large", "small"), 50)
item <- rep(rep(1:5), 20)

my_data <- arrange(as.tibble(cbind(subject, condition, item)), -desc(condition))
my_data <- arrange(my_data, desc(item))

set.seed(1234)
rt <- c(rnorm(10, 800, 100), rnorm(10, 1000, 100),
        rnorm(10, 800, 100), rnorm(10, 1000, 100),
        rnorm(10, 800, 100), rnorm(10, 1000, 100),
        rnorm(10, 800, 100), rnorm(10, 1000, 100),
        rnorm(10, 800, 100), rnorm(10, 1000, 100))

fulldata <- arrange(cbind(my_data, rt), -desc(subject))

set.seed(1)
baseline <- c(rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5))

fulldata <- fulldata %>% arrange(subject, desc(condition))

fulldata$rt <- fulldata$rt + baseline 

mixed_model <- lmer(rt ~ condition + (1 | subject) + (1 | item), data = fulldata)
summary(mixed_model)

mixed_model_null <- lmer(rt ~ (1 | subject) + (1 | item), data = fulldata)
summary(mixed_model_null)

anova(mixed_model, mixed_model_null)

coef(mixed_model)

mixed_model <- lmer(rt ~ condition + (1 + condition | subject) + (1 + condition | item), data = fulldata)
summary(mixed_model)

mixed_model_null <- lmer(rt ~ (1 + condition | subject) + (1 + condition | item), data = fulldata)
summary(mixed_model_null)

anova(mixed_model, mixed_model_null)

coef(mixed_model)

# Plot intercepts and slopes ####
# First by subjects ####
intercepts <- coef(mixed_model)$subject[1]
slopes <- coef(mixed_model)$subject[2]

large <- unlist(intercepts, recursive = TRUE, use.names = F)
small <- unlist(intercepts, recursive = TRUE, use.names = F) + 
  unlist(slopes, recursive = TRUE, use.names = F)
rt <- c(large, small)
condition <- c(rep("large", 10), rep("small", 10))

subject <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9, 1, 10, 2, 3, 4, 5, 6, 7, 8, 9)

my_data <- arrange(as.tibble(cbind(subject, condition, rt)), -desc(subject))

my_data$rt <- as.integer(my_data$rt)
my_data$condition <- as.factor(my_data$condition)
my_data$subject <- as.factor(my_data$subject)

my_data %>% group_by(condition) %>% summarise(mean(as.integer(rt)))

ggplot(my_data, aes (x = condition, y = rt, group = subject, label = subject)) + 
  geom_line(alpha = .5) +
  geom_text(check_overlap = T) +
  labs(title = "Individual model slopes labelled per subject",
       x = "Condition", 
       y = "RT in ms.")

# Now by items ####
intercepts <- coef(mixed_model)$item[1]
slopes <- coef(mixed_model)$item[2]

large <- unlist(intercepts, recursive = TRUE, use.names = F)
small <- unlist(intercepts, recursive = TRUE, use.names = F) + 
  unlist(slopes, recursive = TRUE, use.names = F)
rt <- c(large, small)
condition <- c(rep("large", 5), rep("small", 5))

item <- c(1:5, 1:5)

my_data <- arrange(as.tibble(cbind(item, condition, rt)), -desc(item))

my_data$rt <- as.integer(my_data$rt)
my_data$condition <- as.factor(my_data$condition)
my_data$item <- as.factor(my_data$item)

my_data %>% group_by(condition) %>% summarise(mean(as.integer(rt)))

ggplot(my_data, aes (x = condition, y = rt, group = item, label = item)) + 
  geom_line(alpha = .5) +
  geom_text(check_overlap = T) +
  labs(title = "Individual model slopes labelled per item",
       x = "Condition", 
       y = "RT in ms.")

# Comparing model subject intercept and slopes to the actual data
intercepts <- coef(mixed_model)$subject[1]
slopes <- coef(mixed_model)$subject[2]
model_estimates <- as.tibble(cbind(intercepts, slopes))
model_estimates$subject <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
colnames(model_estimates) <- c("estimated_large", "estimated_small", "subject")

model_estimates <- transmute(model_estimates, subject = as.character(subject), 
                             estimated_large, estimated_small = estimated_large + 
                               estimated_small)

# reshaping the actual data 
agg_data <- spread(fulldata, key = "condition", value = "rt")

# bring together the model estimates with the actual data
together <- full_join(model_estimates, agg_data, by = "subject")

# plot the actual data against the data predicted by the model
together %>% 
  ggplot(aes(x = estimated_large, y = large)) + 
  geom_point() +
  #geom_smooth(method = "lm") +
  labs(x = "Predicted by the model", y = "Actual values", 
       title = "Actual data and data predicted by the model for participants")

# Checking fit of residuals against fitted values
plot(fitted(mixed_model), residuals(mixed_model), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Model Fit for both participants and items random effects",
     col = "red")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(mixed_model), residuals(mixed_model)))

# Checking out the residuals
qqnorm(residuals(mixed_model))
qqline(residuals(mixed_model))

# Illustration of partial pooling ####
# Inspired bt Tristan Mahr’s worked example: 
# https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/

subject <- rep(1:10, each = 5)
caffeine <- rep(rep(1:5), 10)

data <- arrange(as.tibble(cbind(subject, caffeine)), caffeine)

set.seed(1234)
rt <- c(rnorm(10, 1300, 50),
        rnorm(10, 1200, 55),
        rnorm(10, 1100, 55),
        rnorm(10, 1000, 60),
        rnorm(10, 1000, 80))

data_all <- arrange(cbind(data, rt), subject)

data_all[subject == 10 & caffeine != 1,]$rt <- NA

ggplot(data_all, aes (x = caffeine, y = rt)) + 
  geom_point() +
  geom_smooth(data = data_all, method = "lm", se = FALSE, colour = "#00BFC4", size = .75, linetype = 1) +
  facet_wrap(subject, nrow = 2) +
  labs(x = "Caffeine", y = "RT") + 
  theme(legend.position = "top")

no_pooling <- as.tibble(lmList(rt ~ caffeine | subject, data_all) %>% 
                          coef() %>% 
                          # Subject IDs are stored as row-names. Make them an explicit column
                          rownames_to_column("subject") %>% 
                          rename(Intercept = `(Intercept)`, Slope = caffeine) %>% 
                          add_column(Model = "No pooling"))

no_pooling$subject <- as.integer(no_pooling$subject)

model_pooled <- lm(rt ~ caffeine, data_all) 

# Repeat the intercept and slope terms for each participant
pooled <- data_frame(
  Model = "Complete pooling",
  subject = as.integer(unique(data_all$subject)),
  Intercept = coef(model_pooled)[1], 
  Slope = coef(model_pooled)[2])

# Join the raw data so we can use plot the points and the lines.
models <- bind_rows(pooled, no_pooling) %>% 
  left_join(data_all, by = "subject")

models$subject <- as.factor(models$subject)

p_model_comparison <- ggplot(models) + 
  aes(x = caffeine, y = rt) + 
  geom_abline(aes(intercept = Intercept, slope = Slope, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap(~ subject, nrow = 2) +
  labs(x = "Caffeine", y = "RT") + 
  theme(legend.position = "top")

p_model_comparison

m <- lmer(rt ~ 1 + caffeine + (1 + caffeine | subject), data_all)

partial_pooling <- coef(m)[["subject"]] %>% 
  as_tibble() %>% 
  rownames_to_column("subject") %>% 
  rename(Intercept = `(Intercept)`, Slope = caffeine) %>% 
  add_column(Model = "Partial pooling")

partial_pooling$subject <- as.integer(partial_pooling$subject)

df_models <- bind_rows(pooled, no_pooling, partial_pooling) %>% 
  left_join(data_all, by = "subject")

# Replace the data-set of the last plot
p_model_comparison %+% df_models

# Linear mixed model (LMM) for 1-factor (repeated measures) with three levels ####
DV <- read_csv("data_files/DV_1factor.csv")

DV$Condition <- as.factor(DV$Condition)
DV$Condition <- relevel(DV$Condition, ref = 3)

ggplot(DV, aes(x = Condition, y = Gaze, colour = Condition)) + 
  geom_boxplot() + 
  guides(colour = FALSE)

ggplot(DV, aes(x = Condition, y = Gaze, colour = Condition)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .5) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") + 
  guides(colour = FALSE)

model.null <- lmer(Gaze ~ (1 + Condition | Subject) + (1 + Condition|Item), 
                   data = DV, REML = TRUE)

model.full <- lmer(Gaze ~ Condition + (1 + Condition | Subject) + (1 + Condition | Item), 
                   data = DV, REML = TRUE)

anova(model.null, model.full)
summary(model.full)

# GLMM for binomial data - 1-factor with(repeated measures) with 3 levels 
RO <- read_csv("RO.csv", col_types = cols(Condition = col_factor(levels = c("Neutral", 
                                                                            "Positive", "Negative"))))

data_agg <- RO %>% 
  group_by(Condition) %>% 
  summarise(mean=mean(DV), sd=sd(DV))

ggplot(data_agg, aes(x = Condition, y = mean, fill = Condition)) + 
  geom_col() +
  guides(fill = FALSE)

model.full <- glmer(DV ~ Condition + (1 + Condition | Subject) +
                      (1 + Condition | Item), data = RO, family = binomial)

model.interceptonly <- glmer(DV ~ Condition + (1 | Subject) + (1 | Item), 
                             data = RO, family = binomial)

model.null <- glmer(DV ~ (1 | Subject) + (1 | Item), data = RO, family = binomial)

anova(model.interceptonly, model.null)

# LMM for 2x2 repeated measures factorial design ####
DV <- read_csv("data_files/DV.csv")

DV$Sentence <- as.factor(DV$Sentence)
DV$Context <- as.factor(DV$Context)

# Set up contrast coding
contrasts(DV$Sentence) <- matrix(c(.5, -.5))
contrasts(DV$Context) <- matrix(c(.5, -.5))

# Visualise
ggplot(DV, aes(x = Context:Sentence, y = RT, colour = Context:Sentence)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .1) + 
  stat_summary(fun.data = "mean_cl_boot", colour="black") + 
  guides(colour = FALSE)

data_agg <- DV %>% 
  group_by(Context, Sentence) %>% 
  summarise_at("RT", funs(mean, sd), na.rm=TRUE)

data_agg$SE <- data_agg$sd/sqrt(60)

# Not used
# ggplot(data_agg, aes(x=Context:Sentence, y=mean, fill=Context:Sentence)) + geom_col() + 
#  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE, width=.5)) + guides(fill=FALSE)

ggplot(data_agg, aes(x = Context, y = mean, group = Sentence, colour = Sentence)) + 
  geom_line() + 
  geom_point() + 
  ylim(1200,1800) + 
  labs(title = "Plot of RT by Condition with SE bars", y = "Mean RT (ms.)") +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE, width = .1)) + 

model.full <- lmer(RT ~ Context * Sentence + (1 + Context * Sentence | Subject) + 
                    (1 + Context * Sentence | Item), data = DV, REML = TRUE)

model.null <- lmer(RT ~ (1 + Context * Sentence | Subject) + 
                     (1 + Context * Sentence | Item), data = DV, REML = TRUE)

anova(model.full, model.null)
summary(model.full)

emmeans(model.full, pairwise ~ Context * Sentence, adjust="none")

# CLMM for ordinal data ####
Main <- read_csv("data_files/Main.csv")

Main$Subject <- as.factor(Main$Subject)
Main$Image <- as.factor(Main$Image)
Main$SportType <- as.factor(Main$SportType)
Main$VideoCondition <- as.factor(Main$VideoCondition)

# VideoCondition 2=congruent, 3=Incongruent, 4=neutral
Main$VideoCondition <- recode(Main$VideoCondition, 
                               "2"="Congruent", "3"="Incongruent", "4"="Neutral")

Main %>%
  ggplot(aes(x = VideoCondition, y = ratings, colour = VideoCondition)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .2) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE) +
  labs( x = "Condition", y = "Rating", title = "Rating Scores by Condition")

# Set up model - first need to set our DV as an ordered factor
Main$ratings <- as.ordered(Main$ratings)
model.clm.null <- clmm(ratings ~ 1 + (1 + VideoCondition|Subject) + 
                         (1 + VideoCondition|SportType) + 
                         (1 + VideoCondition|Image), data = Main)

model.clm4 <- clmm(ratings ~ VideoCondition + (1 + VideoCondition|Subject) + 
                     (1 + VideoCondition|SportType) + 
                     (1 + VideoCondition|Image), data = Main)

anova(model.clm.null, model.clm4)

emmeans(model.clm4, pairwise ~ VideoCondition, adjust = "none")

# Build and examine normality of residuals with data untransformed
model.full <- lmer(RT ~ Context * Sentence + (1 + Context + Sentence | Subject) + 
                     (1 + Context + Sentence | Item), data = DV, REML = TRUE)

# checking residuals at the model level
qqnorm(residuals(model.full))
qqline(residuals(model.full))
summary(model.full)

# checking residuals at the random effects level
r_int <- lme4::ranef(model.full)$Subject$`(Intercept)`
qqnorm(r_int)
qqline(r_int)

r_slope <- lme4::ranef(model.full)$Subject$Context1
qqnorm(r_slope)
qqline(r_slope)

r_slope <- lme4::ranef(model.full)$Subject$Sentence1
qqnorm(r_slope)
qqline(r_slope)

# Build and examine normality of residuals with data log transformed 
model.full <- lmer(log(RT) ~ Context * Sentence + (1 + Context * Sentence | Subject) + 
                     (1 + Context * Sentence | Item), data = DV, REML = TRUE)

qqnorm(residuals(model.full))
qqline(residuals(model.full))
summary(model.full)

# Build and examine normality of residuals with GLMM under the Gamma distribution - 
# simplified random effects structure needed to converge
model.full <- glmer(RT ~ Context * Sentence + (1 + Context | Subject) + 
                      (1 + Context | Item), data = DV, family = Gamma)

qqnorm(residuals(model.full))
summary(model.full)

