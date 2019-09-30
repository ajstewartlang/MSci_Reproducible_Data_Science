library(tidyverse) # Usual tidyverse packages
library(broom) # Use to tidy results of t-tests into tibbles
library(gamlss) # Needed to sample from ex-Gaussian distribution
library(MASS) # Need to sample from multivariate distribution
library(Hmisc) # For working out Pearson's r
library(grid) # Needed for rasterGrob

# We can use the rnorm() function to sample x times from a normal distribution where we specify
# the mean and the sd.  

rnorm(5, 0, 1)
rnorm(5, 0, 1)

set.seed(1234)
rnorm(5, 0, 1)
set.seed(1234)
rnorm(5, 0, 1)

# As we haven't used set.seed() the random samples are different from each other. Below we use
# set.seed() to ensure we can reproduce the output.

# Here we sample 50 times from a normal distribution with a mean of zero 
# and sd of 1.  We use the set.seed() function so we can reproduce this random sample.

set.seed(1234)
dist <- rnorm(50, 0, 1)
hist(dist)

# The larger the sample, the more representative it is of the popuation it's drawn from.

set.seed(1234)
dist <- rnorm(5000000, 0, 1)
hist(dist, breaks = 1000)

dist <- as.tibble(dist)
ggplot(dist, aes(x = value)) + geom_histogram(bins = 1000) + 
  geom_vline(xintercept = 1.96, colour = "red") +
  geom_vline(xintercept = -1.96, colour = "red") +
  ylab("Density") + 
  xlab("Z-Score") + 
  scale_y_discrete(labels = NULL)

# In the standard normal distribution, 95% of the data is contained with 1.96 standard deviations
# either side of the mean.  

# We can use the prnorm() function to give us the area under the curve of the normal distribution.
# To work out the area bounded by 1.96 standard deviations from the mean in the standard normal
# distribution.
pnorm(1.96, mean = 0, sd = 1) - pnorm(-1.96, mean = 0, sd = 1)

# So the are in the tails must be 1 minus this number as the total area under the curve is equal to 1
1 - pnorm(1.96, mean = 0, sd = 1) - pnorm(-1.96, mean = 0, sd = 1)

# We can convert any normally distributed data to the standard normal
# by using the scale() function.

set.seed(1234)
dist1 <- rnorm(50000, 1000, 50)
hist(dist1, breaks = 100)
hist(scale(dist1), breaks = 100)

set.seed(1234)
cond1 <- rnorm(1000000, 0, 1)
cond2 <- rnorm(1000000, 1.96, 1)
data <- as.tibble(cbind(cond1, cond2))

ggplot(data) + geom_density(aes(x = cond1, y = ..density.., colour = "red")) +
  geom_density(aes(x = cond2, y = ..density.., colour = "green")) + 
  xlab("Data") + 
  guides(colour = FALSE)

# First let's simulate data from a one factor between participants experiment.
# Each of the 24 participant will have 1 measures - one for each level of the factor.

# First let's create a vector for our participants.  It will range from 1 to 24

participant <- rep(1:24)

# We can check we have what we want by typing the variable name

participant

# Now we need to creaee the conditions - Condition 1 we will label 'fast' and Condition 2 we will label 'slow'
# We use the c() function to combine the arguments that follow it (i.e., "fast" and "slow") into a vector.
# The first 12 participants are in the "fast" condition, and the second 12 the "slow"

condition <- c(rep("fast", times = 12), rep("slow", times = 12))

# We now have "fast" and "slow" repeated 12 times each - test by typing "condition"

condition

# Now we need to simulate our data - we wil assume we're sampling from the normal distribution so wil
# use the rnorm function.  This selects samples from a normal distribution where we specify the mean
# and sd.  We want to simulate the data for our "fast" condition as coming from a distribution with a 
# mean = 1000 and sd = 50, and the data for our "slow" condition from a distribution with a mean = 1020
# and sd = 50. We need to make sure we set up our sampling using the rnorm() function in the same wasy 
# as we did for specifying the condition variable.

# To make sure we can reproduce these random samples in future, we can use the function set.seed()
# to specify the start of the random number generation.

set.seed(1234)
dv <- c(rnorm(12, 1000, 50), rnorm(12, 1020, 50))
dv

# We now need to combined our 3 columns into a tibble. We use the cbind() function to first bind the three
# variables together as columns, and then as.tibble() to convert these three combined columns to a tibble.
# A tibble is really just a supercharged dataframe.

my_data <- as.tibble(cbind(participant, condition, dv))
my_data

# Let's check to make sure our data look as we expect. We need to turn our condition column in the tibble 
# into a factor.

my_data$condition <- as.factor(my_data$condition)

# We also have mamy decimal places in our dependent variable so let's drop those by converting this column
# to integers.

my_data$dv <- as.integer(my_data$dv)

ggplot(my_data, aes(x = condition, y = dv, fill = condition)) + 
  geom_violin() + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  geom_jitter(alpha = .2, width = .05) + 
  guides(fill = FALSE)

# Looks petty good!  We can now perform an independent sample t-test to see if the conditons differ

t.test(filter(my_data, condition == "fast")$dv, filter(my_data, condition == "slow")$dv, paired = FALSE)

# We can also save the result of this t-test using the broom::tidy() function.  This converts the 
# output of the t-test into a tidy tibble.

result <- tidy(t.test(filter(my_data, condition == "fast")$dv, 
                      filter(my_data, condition == "slow")$dv, paired = FALSE))

# We can reference columns in this table - for example, just to get the p-value:

result$p.value

# Introducing the for loop

# Printing something i times

for (i in 1:10){
  print("Hello world")
}

# Simply printing i as it increments

for (i in 1:10){
  print(i)
}

# Printing i + 10

for (i in 1:10){
  print(i+10)
}

# Printing i as part of a string

for (i in 1:10){
  print(paste("This is the number", i, sep=" "))
}

# Indexing a vector

for (i in 1:10){
  print(my_data$dv[i])
}

# Simulating 100 experiments
total_samples <- 100
sample_size <- 24
participant <- rep(1:sample_size)
condition <- c(rep("fast", times = sample_size/2), rep("slow", times = sample_size/2))
all_data <- NULL

for (i in 1:total_samples) {
  sample <- i
  set.seed(1233 + i)
  dv <- c(rnorm(sample_size/2, 1000, 50), rnorm(sample_size/2, 1020, 50))
  data <- as.tibble(cbind(participant, condition, dv, sample))
  all_data <- rbind(data, all_data)
}

all_data$condition <- as.factor(all_data$condition)

all_data$dv <- as.integer(all_data$dv)

ggplot(all_data, aes(x = condition, y = dv, fill = condition)) + 
  geom_violin() + 
  geom_jitter(alpha = .3, width = .05) + 
  guides(fill = FALSE) + 
  facet_wrap(~ sample)

str(all_data)

all_data %>% group_by(condition, sample) %>% 
  summarise(average = mean(dv), sd(dv)) %>%
  ggplot(aes(x = condition, y = average, group = condition, label = sample)) + 
  geom_jitter(width = .1, alpha = .5) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "blue") + 
  geom_text(check_overlap = TRUE, nudge_x = .2, nudge_y = 0, colour = "black") +
  ylab("Reaction Time (ms.)")

all_data %>% group_by(condition, sample) %>% summarise(mean(dv), sd(dv))

# Saving the p-values for t-tests for each of the samples in one new tibble called "result".

result <- NULL
for (i in 1:total_samples) {
  result <- rbind(tidy(t.test(filter(all_data, condition == "fast" & sample == i)$dv, 
                        filter(all_data, condition == "slow" & sample == i)$dv, paired = FALSE)), result)
}

# All the p-values for each of the 100 tests are stored in the column labelled "p.value" in the
# tibble "result".

# We can plot a histogram of these p-values.

ggplot(result, aes(x = p.value)) + geom_histogram(bins = 50)

# and also work out how many are < .05

count(filter(result, p.value < .05))

# So for a Cohen's d of .4 and with 24 participants in our design we are finding
# a p-value of < .05 17 times out of 100 simulations.  Our estimated power is 17% 
# so less than a fifth of the time are we finding the effect even though it exists in the 
# populations we're comparing.  

# And if we zoom in to visualise only those values where p < .05 we can see a few very small
# p-values.

ggplot(filter(result, p.value < .05), aes(x = p.value)) + geom_histogram(bins = 50)

# We actually need about 200 participants to give us 80% power with a Cohen's d = .4  
# Assign the number 200 (rather than 24) to the sample_size variable.  Run the 
# simulations and you'll see 80 times out of 100 we are finding the effect being significant. Note in the
# plot we still have some non-significant results with one giving us a p-value of almost .6 - even
# though the difference is present in the populations we're sampling from.  

# Relationship between p-values and Cohen's d (tl;dr there isn't one) ####
# The following very messy code works out Cohen's d for each of the sample where
# the t-test is significant.

result_data <- as.tibble(filter(cbind(seq(1:100), result), result$p.value < .05))

colnames(result_data)[1] <- "sample"

temp <- all_data %>% 
          group_by(sample, condition) %>% 
          summarise(mean = mean(dv))

temp <- spread(temp, "condition", "mean", c("fast", "slow"))

temp1 <- all_data %>% 
  group_by(sample, condition) %>% 
  summarise(sd = sd(dv))

temp1 <- spread(temp1, "condition", "sd", c("fast", "slow"))

temp2 <- inner_join(temp, temp1, by = "sample")
colnames(temp2) <- c("sample", "fast_mean", "slow_mean", "fast_sd", "slow_sd")

temp2$sample <- as.integer(temp2$sample)
temp2$fast_mean <- as.integer(temp2$fast_mean)
temp2$slow_mean <- as.integer(temp2$slow_mean)
temp2$fast_sd <- as.integer(temp2$fast_sd)
temp2$slow_sd <- as.integer(temp2$slow_sd)

temp3 <- left_join(result_data, temp2, by = "sample")

temp4 <- temp3 %>% 
  mutate(d = (slow_mean-fast_mean)/sqrt(mean(c((slow_sd*slow_sd), (fast_sd*fast_sd)))))

ggplot(temp4, aes (x = d)) + 
  geom_histogram(bins = 30) + 
  xlab("Cohen's d estimate") + 
  geom_vline(xintercept = .4, colour = "red") + 
  xlim(-.2, 1.1)

ggplot(temp4, aes (x = p.value, y = d)) + 
  geom_point() + 
  geom_smooth(method = "lm")

rcorr(temp4$p.value, temp4$d)
  
# End of worst messy code ever.

# In addition to simulating sampling from the normal, our measure might be better thought of
# as involving sampling froma  different shaped distributions - e.g., ex-Gaussian for reaction time
# data.

a <- rnorm(100000, 1000, 50)
hist(a, breaks = 100)

b <- rexGAUS(100000, mu = 1000, sigma = 100, nu = 500)
hist(b, breaks = 100)


# Above we've effectively run the same code twice, once for sample_size = 24 and once for sample_size = 99
# We could actually turn the code into a function which we call from our script and pass the 
# sample size parameter to the function when we call it....

just_simulate <- function(sample_size) {
    a <- rnorm(sample_size, 0,1)
    hist(a)
}

just_simulate(100000)

simulate <- function(sample_size) { 
  total_samples <- 100
  participant <- rep(1:sample_size, times = 2)
  condition <- c(rep("fast", times = sample_size), rep("slow", times = sample_size))
  all_data <- NULL

  for  (i in 1:total_samples) {
    sample <- i
    set.seed(1233 + i)
    dv <- c(rnorm(sample_size, 1000, 50), rnorm(sample_size, 1020, 50))
    data <- as.tibble(cbind(participant, condition, dv, sample))
    all_data <- rbind(data, all_data)
  }

  all_data$condition <- as.factor(all_data$condition)
  all_data$dv <- as.integer(all_data$dv)
  print(ggplot(all_data, aes(x = condition, y = dv, fill = condition)) + geom_violin() + 
  geom_jitter(alpha = .3, width = .05) + guides(fill = FALSE) + facet_wrap(~sample))
  result <- NULL

  for  (i in 1:total_samples) {
    result <- rbind(tidy(t.test(filter(all_data, condition == "fast" & sample == i)$dv, 
                                filter(all_data, condition == "slow" & sample == i)$dv, 
                                paired = FALSE)), result)
  }

  print(ggplot(result, aes(x = p.value)) + geom_histogram(bins = 50))
  print(ggplot(filter(result, p.value < .05), aes(x = p.value)) + geom_histogram(bins = 50))
  return(count(filter(result, p.value < .05)))
  
}

simulate(24)
simulate(100)

# What if your response variable isn't likely to be normal?

hist(rexGAUS(10000, mu = 0, sigma = 1), breaks = 100)

# Simulating multivariate data with specific covariance structure ####
# Use the mvrnorm() function from the MASS package

# The number of samples we want
n <- 500

# A vector of means of our two variables
mu <- c(1000, 2000) 

# Covariance our 2 variables
# It is equal to Pearson's R * SD_var1 * SD_var2
# If we know the variance for each of our variables we can calculate the sd
# We can then use these values to work out the covariance we need for any particular Pearson's r value
# For the below example to give us a Peasron's r of .5 and variance for var1 = 100,
# and variance of var2 = 50
# we have covariance = .5 * sqrt(100) * sqrt(50) which gives us 35.35534
myr <- 35.35534

# The covariance matrix where we have the variance of variable 1, the covariance of variables 1 and 2
# the covariance of variables 1 and 2 and the variance of variable 2
mysigma <- matrix(c(100, myr, myr, 50), 2, 2) 

# We can set the empricial parameter in mvrnorm to be to TRUE so our  covariance is
# identical to the covariance we specify If we do not set it to be
# TRUE, sampling error will mean we likely end up with a correlation a bit different from what 
# we worked out.
set.seed(1234)
my_data <- data.frame(mvrnorm(n, mu, mysigma, empirical = TRUE))
colnames(my_data) <- c("Var_1", "Var_2")

ggplot(my_data, aes(x = Var_1, y = Var_2)) + 
  geom_point() + 
  geom_smooth(method = "lm")

rcorr(my_data$Var_1, my_data$Var_2)

# Animating sampling many times and illustrating varation in our regression line (and our correlation
# calculation) due to sampling error.
my_data <- NULL
sample_size <- 50

set.seed(1234)
for (i in 2:10) {
  sample <- data.frame(mvrnorm(sample_size, mu, mysigma))
  sample$sample <- i
  my_data <- rbind(cbind(sample, rcorr(sample$X1, sample$X2)$r[2]), my_data)
}

# Now set the first sample so the correlation is identical to the correlation in the multivariate
# population we're sampling from
# Need to reset empirical to TRUE for this to work
sample <- data.frame(mvrnorm(sample_size, mu, mysigma, empirical = FALSE))
sample$sample <- 1
my_data <- rbind(cbind(sample, rcorr(sample$X1, sample$X2)$r[2]), my_data)

colnames(my_data) <- c("Var_1", "Var_2", "Sample", "Correlation")

ggplot(my_data, aes(x = Var_1, y = Var_2)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Sample, ncol = 5, nrow = 2) + 
  labs(title = "10 Samples When Sample Size = 50") + 
  theme(title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

my_data %>% group_by(Sample) %>% summarise(Correlation = mean(Correlation))

ggplot(my_data, aes(x = Correlation)) + geom_histogram(bins = 10)

ggplot(my_data, aes(x = Var_1, y = Var_2)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  transition_time(Sample) + 
  labs(x = "Variable 1", 
       y = "Variable 2", 
       title = "10 samples where each sample size = 500\nSample number: {as.integer(frame_time)}") + 
  theme(title = element_text(size = 15)) +
  ease_aes('linear')

# Hockey game simluation
library(tidyverse)
library(gganimate)
set.seed(1234)

team_b_goals <- NULL

for(i in 1:100000) {
  score <- sum(sample(c(1, 0), size = 20, replace = TRUE, prob = c(0.055, 1-.055)))
  team_b_goals <- c(team_b_goals, score)}

team_a_goals <- rep(1, 100000)

all_games <- as_tibble(cbind(team_a_goals, team_b_goals))

# Calculate cumulative sum
all_games <- all_games %>% 
  mutate(team_a_wins = team_a_goals > team_b_goals,
         team_b_wins = team_b_goals > team_a_goals) 

all_games$game_number <- seq(1:100000)
all_games$team_a_wins <- cumsum(all_games$team_a_wins)
all_games$team_b_wins <- cumsum(all_games$team_b_wins)

small_data <- tibble(c(all_games$team_a_wins, all_games$team_b_wins), 
                    c(rep("Team_A", 100000), rep("Team_B", 100000)),
                    c(seq(1:100000), seq(1:100000)))

colnames(small_data) <- c("Points", "Team", "Game")

small_data %>% 
  filter(Game < 10000) %>% # remove this line to plot all 100000 games - crashes my Mac though!
  ggplot(aes(x = Points, y = Game, group = Team, colour = Team)) +
  geom_point() +
  geom_line(size = 2) + 
  coord_flip() +
  transition_reveal(Game) +
  shadow_mark(past = TRUE) +
  labs(title = "Wins of Team A vs. Team B", 
       x = "Running total of wins",
       y = "Game number") +
  theme(text = element_text(size = 15))

# gganimate demonstrations ####
library(gganimate)
# see animations.R script

# Visualising Likert scale data ####
library(HH)

# Create the data
my_data <- as.data.frame(matrix(c(34,7,13,1,4,13,7,84,24,7,2,27,51,3,23), nrow = 3))
colnames(data) <- c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")
rownames(data) <- c("Question 1", "Question 2", "Question 3")

# Plot the data as divergent stacked bar chart
likert(my_data, 
       main = "Diverging Stacked Bar Chart\nfor Likert Scale Data",
       sub = "Response")

# Scraping Twitter and Visualising Text Data ####
# Doing a quick sentiment analysis on tweets mentioning suicide
# created between midnight and 6AM 

library(rtweet)
library(tidytext)
library(lubridate)

tweets <- search_tweets(q = "suicide", n = 1000, include_rts = FALSE, 
                        retryonratelimit = TRUE)

time <- tibble(Time = hour(tweets$created_at))

time %>%
  filter(!is.na(Time)) %>%
  group_by(Time) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 

time %>%
  filter(!is.na(Time)) %>%
  ggplot(aes(x = Time)) + 
  geom_histogram(binwidth = 1)

time_more <- mutate(tweets, Time = hour(created_at))

text <- filter(time_more, Time > 0 & Time < 6)

rt1 <- text %>%
  unnest_tokens(word, text)

sent <- get_sentiments("bing")   #nrc for details - bing for positive vs negatives sentiment

word_counts <- rt1 %>%
  inner_join(sent) %>%
  count(word, sentiment, sort = TRUE)

word_counts %>% 
  filter(n > 25 & word != "suicide") %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")  

# Creating a wordcloud ####
library(wordcloud)

word_counts_tidy <- filter(word_counts, word != "suicide")

set.seed(1234)

wordcloud(words = word_counts_tidy$word, 
          freq = word_counts_tidy$n, 
          min.freq = 5,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors=brewer.pal(8, "Dark2"))

# More Twitter scraping - Opeth ####
tweets <- search_tweets("Opeth", n = 200000, include_rts = FALSE, retryonratelimit = TRUE) 

tweets <- tweets %>% separate(col = created_at, into = c("date", "time"), sep = " ") 

img <- jpeg::readJPEG("opeth2.jpg")
g <- rasterGrob(img, interpolate=TRUE) 

ggplot(tweets, aes (x = date)) + 
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_bar(fill = "white", alpha = .5) + 
  labs(x = "Date", y = "Number of Tweets") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste("Tweets Mentioning Opeth Scraped On", Sys.Date())) 

# plotting Tweets on map ####
library(leaflet)
my_map <- lat_lng(tweets)
to_plot <- leaflet(my_map) %>% 
  addTiles()

to_plot %>% addCircles(lng = ~lng, lat = ~lat, weight = 8, radius = 40, 
                       color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)

text <- tweets

rt1 <- text %>%
  unnest_tokens(word, text)

sent <- get_sentiments("bing")   # bing for positive vs negatives sentiment

word_counts <- rt1 %>%
  inner_join(sent) %>%
  count(word, sentiment, sort = TRUE)

word_counts %>% 
  filter(n > 25 & word != "suicide") %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")  

# get tweets from two journals - Science and Nature ####
tmls <- get_timelines(c("Nature", "sciencemagazine"), n = 1000)
tmls %>%
  filter(created_at > "2018-12-1") %>%
  group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  geom_point() +
  theme_minimal() +
  theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by the journals Nature and Science",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# get tweets from Trump ####
tmls <- get_timelines(c("@realDonaldTrump"), n = 10000)

text <- tmls

rt1 <- text %>%
  unnest_tokens(word, text)

sent <- get_sentiments("bing")   #nrc for details - bing for positive vs negatives sentiment

word_counts <- rt1 %>%
  inner_join(sent) %>%
  count(word, sentiment, sort = TRUE)

word_counts %>% 
  filter(n > 5 & word != "trump") %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment", 
       title = "Sentiment Analysis of Tweets by @realDonaldTrump over Christmas")  

word_counts_tidy <- word_counts
set.seed(1234)
wordcloud(words = word_counts_tidy$word, 
          freq = word_counts_tidy$n, 
          min.freq = 1,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors=brewer.pal(8, "Dark2"))

tmls %>%
  ts_plot("days", trim = 1L) +
  geom_point() +
  theme_minimal() +
  theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter posts by @realDonaldTrump over Christmas",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# More tidytext ####
library(gutenbergr)
library(wordcloud)

# Get 2 HG Wells books ####
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds")
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

text_waroftheworlds <- books %>%
  filter(title == "The War of the Worlds") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

text_underthesea <- books %>%
  filter(title == "Twenty Thousand Leagues under the Sea") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

text_waroftheworlds %>%
  count(word) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(title = "Top 10 word in The War of the Worlds")
  
text_underthesea_count <- text_underthesea %>%
  count(word) %>%
  top_n(200)

wordcloud(words = text_underthesea_count$word, 
          freq = text_underthesea_count$n, 
          min.freq = 1,
          scale = c(3, 1), 
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

# BBC plots ####
devtools::install_github('bbc/bbplot')

library(tidyverse)
library(bbplot)

my_plot <- mpg %>%
  count(class) %>%
  mutate(class = fct_reorder(class, n)) %>%
  ggplot(aes(x = class, y = n, fill = class)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) 

my_plot <- mpg %>%
  count(class) %>%
  mutate(class = fct_reorder(class, n)) %>%
  ggplot(aes(x = class, y = n, fill = class)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE)  +
  bbc_style() +
  labs(title="Number of each type of car",
       subtitle = "Data collected in the US, 1999-2008")

finalise_plot(plot_name = my_plot,
              source = "ONS",
              save_filepath = "my_plot.png",
              width_pixels = 640,
              height_pixels = 550)
