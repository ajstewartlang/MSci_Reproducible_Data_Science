# We'll start by loading the packages we need.

library(tidyverse) #load the tidyverse packages
library(afex) #load afex for running factorial ANOVA
library(emmeans) #load emmeans for running pairwise comparisons

# ANOVA - one factor, three levels between participants design ####

cond <- read_csv("data_files/cond.csv")

cond$Condition <- as.factor(cond$Condition)

cond %>% 
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme(text = element_text(size = 15))

cond %>%
  group_by(Condition) %>%
  summarise(mean = mean(Ability), sd = sd(Ability))

model <- aov_4(Ability ~ Condition + (1 | Participant), data = cond)

summary(model)

emmeans(model, pairwise ~ Condition)

# Repeated Measures ANOVA - one factor with 4 levels ####

rm_data <- read_csv("data_files/rm_data.csv")
rm_data$Condition <- as.factor(rm_data$Condition)
rm_data

rm_data %>%
  group_by(Condition) %>%
  summarise(mean = mean(Score), sd = sd (Score))

rm_data %>%
  ggplot(aes(x = Condition, y = Score, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme(text = element_text(size = 15))

rm_data %>%
  ggplot(aes(x = fct_reorder(Condition, Score), y = Score, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme(text = element_text(size = 15)) +
  labs(x = "Condition")

model <- aov_4(Score ~ Condition + (1 + Condition | Participant), data = rm_data)
summary(model)
anova(model)

emmeans(model, pairwise ~ Condition, adjust = "Bonferroni")

# 2 x 2 repeated design long data format ####

fact_data <- read_csv("data_files/fact_data.csv")
fact_data$Sentence <- as.factor(fact_data$Sentence)
fact_data$Context <- as.factor(fact_data$Context)

fact_data

fact_data %>%
  group_by(Context, Sentence) %>%
  summarise(mean = mean(RT), sd = sd(RT))

visdat::vis_miss(fact_data)

fact_data %>%
  filter(!is.na(RT)) %>%
  group_by(Context, Sentence) %>%
  summarise(mean = mean(RT), sd = sd(RT))

fact_data %>%
  group_by(Context, Sentence) %>%
  summarise(mean = mean(RT, na.rm = TRUE), sd = sd(RT, na.rm = TRUE))

fact_data %>%
  ggplot(aes(x = Context:Sentence, y = RT, colour = Context:Sentence)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Condition")

# By Subjects ANOVA ####
model_subjects <- aov_4(RT ~ Context * Sentence + (1 + Context * Sentence | Subject), data = fact_data, na.rm = TRUE)

anova(model_subjects)

emmeans(model_subjects, pairwise ~ Context * Sentence, adjust = "none")

# By Items ANOVA ####
model_items <- aov_4(RT ~ Context * Sentence + (1 + Context * Sentence | Item), data = fact_data, na.rm = TRUE)

anova(model_items)

emmeans(model_items, pairwise ~ Context * Sentence, adjust = "none")
