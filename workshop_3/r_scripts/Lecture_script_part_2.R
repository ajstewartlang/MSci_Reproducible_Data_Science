library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons

# Mixed ANOVA with one between participants factor, and two repeated
my_data <- read_csv("my_data.csv")

my_data <- my_data %>% 
  mutate(Image = factor(Image)) %>%
  mutate(Word = factor(Word)) %>%
  mutate(Age = factor(Age))

ggplot(my_data, aes(x = Word:Image, y = RT, colour = Word:Image)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .2) + 
  stat_summary(fun.data = "mean_cl_boot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Age) + 
  guides(colour = FALSE)

model <- aov_4(RT ~ Word * Image * Age + (1 + Word * Image | Participant), data = my_data)
summary(model)
anova(model)

emmeans(model, pairwise ~ Word * Image * Age, adjust = "Bonferroni")

# Three way interaction is significant so run two separate 2-ways
young_filter <- filter(my_data, Age == "Young")

model_young <- aov_4(RT ~ Word * Image + (1 + Word *Image | Participant), data = young_filter)
anova(model_young)

emmeans(model_young, pairwise ~ Word * Image, adjust = "Bonferroni")

old_filter <- filter(my_data, Age == "Old")

model_old <- aov_4(RT ~ Word * Image + (1 + Word * Image | Participant), data = old_filter)
summary(model_old)
anova(model_old)

emmeans(model_old, pairwise ~ Word * Image, adjust = "Bonferroni")

#ANCOVA
cond <- read_csv("cond.csv")

cond <- cond %>% 
  mutate(Condition = as.factor(Condition))
  
ggplot(cond, aes(x = Gaming, y = Ability,  colour = Condition)) + 
  geom_point() 

# Separately by Condition
ggplot(cond, aes(x = Gaming, y = Ability,  colour = Condition)) + 
  geom_point() + 
  facet_wrap(~ Condition) + 
  geom_smooth(method = "lm") +
  guides(colour = FALSE)

# Run the ANOVA (i.e., without the covariate)- model is significant
model1 <- aov_4(Ability ~ Condition + (1 | Participant), data = cond)
anova(model1)

# Run the ANCOVA - when we add the effect of Gaming Frequency first,
# Condition is now not significant
cond$Gaming <- scale(cond$Gaming)
model_ancova <- aov_4(Ability ~ Gaming + Condition + (1 | Participant), data = cond, factorize = FALSE)
anova(model_ancova)

# Unadjusted means
cond %>%
  group_by(Condition) %>%
  summarise(mean_ability = mean(Ability), sd_ability = sd(Ability))

# Report adjusted means
emmeans(model_ancova, pairwise~Condition, adjust = "none")

# ANCOVA as a special case of Regression 
ggplot(cond, aes(x = Condition, y = Ability, colour = Condition)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .5) + 
  stat_summary(fun.data = "mean_cl_boot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(colour = FALSE)

# Set up the Water level as the reference level and check the contrasts
cond$Condition <- relevel(cond$Condition, ref = 3)
contrasts(cond$Condition)

# Build a linear model just with Condition predicting Ability
lm1 <- lm(Ability ~ Condition, data = cond)
lm1

#Build a linear model with both Gaming Frequency and Condition predicting Ability
lm2 <- lm(Ability ~ Gaming + Condition, data = cond)
lm2

