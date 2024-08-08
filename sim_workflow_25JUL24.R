#Clear the workspace
rm(list = ls())


### First, let's use our domain knowledge of afffective constructs (self-efficacy and attention) and gender to 
# buid a scientific model predicting student scores

#Specify a scientific model
library(dagitty)
g0 <- dagitty("dag {
              attention->score}")
plot(graphLayout(g0))

g1 <- dagitty("dag {
             attention->score;
             self_efficacy->score }")
plot(graphLayout(g1))

g2 <- dagitty("dag {
             attention->score;
             self_efficacy->attention;
             self_efficacy -> score}")
plot(graphLayout(g2))

g3 <- dagitty("dag {
             attention->score;
             self_efficacy->attention;
             self_efficacy -> score
              gender->self_efficacy
              gender->attention}")
plot(graphLayout(g3))

# Find the adjustment set for attention -> score
adjustmentSets(g3, exposure = "attention", outcome = "score") #self_efficacy

# Find the adjustment set for self_efficacy -> score
adjustmentSets(g3, exposure = "self_efficacy", outcome = "score") #gender

##### Now we will simulate some data to see if we can recover the effect of attention on score ###

# Simulate n=3000 students clustered in n=100 classrooms with ICC of 0.2
library(MASS)  # For mvrnorm to simulate correlated data
library(lme4)  # For creating random effects to account for ICC

set.seed(666)  # For reproducibility

# Parameters
n_classrooms <- 100
students_per_classroom <- 30
n_students <- n_classrooms * students_per_classroom
ICC <- 0.2
mean_score <- 0
sd_score <- 1

# Generate gender
gender <- rbinom(n = n_students, size = 1, prob = 0.5)

# Simulate self-efficacy and attention using gender effects
beta_gender_self_efficacy <- -0.2
beta_gender_attention <- 0.2
beta_self_efficacy_attention <- 0.1
beta_self_efficacy_score <- 0.2
beta_attention_score <- 0.2

# Generate self-efficacy and attention
self_efficacy <- rnorm(n_students, mean = beta_gender_self_efficacy * gender, sd = 1)
attention <- rnorm(n_students, mean = beta_gender_attention * gender + beta_self_efficacy_attention * self_efficacy, sd = 1)

# Generating clustered score
# Simulate classroom random effects
random_effects <- rnorm(n_classrooms, mean = 0, sd = sqrt(ICC / (1 - ICC)))
classroom_ids <- rep(1:n_classrooms, each = students_per_classroom)
classroom_effect <- random_effects[classroom_ids]

# Generate scores incorporating classroom effects
score <- rnorm(n_students, mean = mean_score + beta_self_efficacy_score * self_efficacy + beta_attention_score * attention + classroom_effect * sd_score, sd = sqrt(1 - ICC))

# Combine into a data frame
data <- data.frame(classroom_id = classroom_ids, gender = gender, self_efficacy = self_efficacy, attention = attention, score = score)

# Display the first few rows of the data
head(data)

# Check to see if a multilevel model can recover the trend (attention -> score)
# We will fit Bayesian mlm via rstanarm

library(rstanarm)
m1 <- stan_lmer(score~1+attention+(1|classroom_id),data=data,
                prior_intercept=normal(0,10),prior=normal(0,10),
                chains=3, cores=3, seed=666)
print(m1, digits=4) #Upwardly biased

m2 <- stan_lmer(score~1+attention+self_efficacy + (1|classroom_id),data=data,
                prior_intercept=normal(0,10),prior=normal(0,10),
                chains=3, cores=3, seed=666)
print(m2, digits=4) #Better, conrolling for self_efficacy (per the DAG)

# Simulation study to determine sample size
# Check to see at what sample size the average "significant" effect overlaps the expected effect

library(MASS)
library(lme4)
library(dplyr)

set.seed(666)

# Parameters
students_per_classroom <- 30
n_simulations <- 1000
max_classrooms <- 20

# Storage for results
results <- data.frame(
  n_classrooms = integer(),
  avg_estimate_standard = numeric(),
  avg_se_standard = numeric(),
  avg_estimate_conditional = numeric(),
  avg_se_conditional = numeric()
)

# Simulation loop
for (n_classrooms in 1:max_classrooms) {
  
  estimates_standard <- numeric(n_simulations)
  ses_standard <- numeric(n_simulations)
  estimates_conditional <- list()
  ses_conditional <- list()
  
  for (sim in 1:n_simulations) {
    # Generate data
    n_students <- n_classrooms * students_per_classroom
    gender <- rbinom(n = n_students, size = 1, prob = 0.5)
    self_efficacy <- rnorm(n_students, mean = -0.2 * gender, sd = 1)
    attention <- rnorm(n_students, mean = 0.2 * gender + 0.1 * self_efficacy, sd = 1)
    random_effects <- rnorm(n_classrooms, mean = 0, sd = sqrt(0.2 / (1 - 0.2)))
    classroom_ids <- rep(1:n_classrooms, each = students_per_classroom)
    classroom_effect <- random_effects[classroom_ids]
    score <- rnorm(n_students, mean = 0 + 0.2 * self_efficacy + 0.2 * attention + classroom_effect * 1, sd = sqrt(1 - 0.2))
    
    # Fit model
    data <- data.frame(score = score, attention = attention, self_efficacy = self_efficacy)
    model <- lm(score ~ attention + self_efficacy, data = data)
    coef_est <- coef(summary(model))["attention", ]
    
    # Store standard estimates and SEs
    estimates_standard[sim] <- coef_est["Estimate"]
    ses_standard[sim] <- coef_est["Std. Error"]
    
    # Conditional estimates and SEs
    if (abs(coef_est["Estimate"]) > coef_est["Std. Error"]) {
      estimates_conditional[[sim]] <- coef_est["Estimate"]
      ses_conditional[[sim]] <- coef_est["Std. Error"]
    }
  }
  
  # Calculate averages
  avg_est_standard <- mean(estimates_standard)
  avg_se_standard <- mean(ses_standard)
  avg_est_conditional <- mean(unlist(estimates_conditional), na.rm = TRUE)
  avg_se_conditional <- mean(unlist(ses_conditional), na.rm = TRUE)
  
  # Store results
  results <- rbind(results, data.frame(
    n_classrooms = n_classrooms,
    avg_estimate_standard = avg_est_standard,
    avg_se_standard = avg_se_standard,
    avg_estimate_conditional = avg_est_conditional,
    avg_se_conditional = avg_se_conditional
  ))
}

# View results
print(results) # Need around 8 classrooms (240 students) for the estimates to have minimum Type M error

# Visualize results!
# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Reshape for plotting
results_long <- results %>%
  pivot_longer(
    cols = starts_with("avg"),  # Selecting columns starting with 'avg'
    names_to = c(".value", "type"),  # Splitting into two columns: one for values, one for types
    names_pattern = "avg_(.+)_(standard|conditional)"  # Correctly splitting the name into measure and type
  ) %>%
  mutate(type = ifelse(type == "standard", "Standard", "Conditional"))  # Making type names more readable

# Plotting
ggplot(results_long, aes(x = n_classrooms, y = estimate, color = type)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), width = 0.1, position = position_dodge(width = 0.2)) +
  labs(title = "Effect of Attention on Score \n Across Different Classroom Sizes",
       x = "Number of Classrooms", y = "Average Estimate of Attention Effect",
       color = "Type of Estimate") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

#Try it on some example data! 8 classes, 240 students
library(readr)
df <- read_csv("sim_df.csv")
head(df)

#### Now let's scale the priors so they're more realistic. +/- 2SD range on score

#prior predictive simulations
set.seed(666)
n <- 20

#ultra-wide priors
a <- rnorm(n,0,10)
bS <- rnorm(n,0,10)

plot(NULL,xlim=c(-3,3), ylim=c(-8,8),
     main = "Ultra-Wide Priors \n a=n(0,10), b=n(0,10)",
     xlab = "predictor score (standardized)",
     ylab = "score")
Xseq <- seq(from=-3, to=3, length=30 )
for ( i in 1:n ) {
  mu = a[i] + bS[i]*Xseq
  lines(Xseq, mu, lwd=2, col=2)
}

#inf priors
a <- rnorm(n,0,1)
bS <- rnorm(n,0,1)

plot(NULL,xlim=c(-3,3), ylim=c(-8,8),
     main = "Informative Priors \n a=n(0,1), b=n(0,1)",
     xlab = "predictor score (standardized)",
     ylab = "score")
Xseq <- seq(from=-3, to=3, length=30 )
for ( i in 1:n ) {
  mu = a[i] + bS[i]*Xseq
  lines(Xseq, mu, lwd=2, col=2)
}

#very inf priors
a <- rnorm(n,0,0.5)
bS <- rnorm(n,0,0.5)

plot(NULL,xlim=c(-3,3), ylim=c(-8,8),
     main = "Very Informative Priors \n a=n(0,0.5), b=n(0,0.5)",
     xlab = "predictor score (standardized)",
     ylab = "score")
Xseq <- seq(from=-3, to=3, length=30 )
for ( i in 1:n ) {
  mu = a[i] + bS[i]*Xseq
  lines(Xseq, mu, lwd=2, col=2)
}

#fit an example bayesian mlm via rstanarm with scaled priors (very informative)
lm <- stan_lmer(score~1+attention+self_efficacy + (1|classroom_id),data=df,
                prior_intercept=normal(0,0.5), prior=normal(0,0.5),
                chains=3,cores=3,seed=666)
lm

#plot of all modeled posteriors
plot(lm) 

#posterior predictive check (we recover the score data fairly well)
pp_check(lm)

#can we recover the "correct model" to estimate the effect of attention on score via loo?

# First we fit a series of models with different combinations of predictors.
t0 <- stan_lmer(score~1+ (1|classroom_id),data=df,
                prior_intercept=normal(0,0.5), prior=normal(0,0.5),
                chains=3,cores=3,seed=666)

t1 <- stan_lmer(score~1+attention + (1|classroom_id),data=df,
                prior_intercept=normal(0,0.5), prior=normal(0,0.5),
                chains=3,cores=3,seed=666)

t2 <- stan_lmer(score~1+attention + self_efficacy + (1|classroom_id),data=df,
                prior_intercept=normal(0,0.5), prior=normal(0,0.5),
                chains=3,cores=3,seed=666)

t3 <- stan_lmer(score~1+attention + self_efficacy + gender + (1|classroom_id),data=df,
                prior_intercept=normal(0,0.5), prior=normal(0,0.5),
                chains=3,cores=3,seed=666)

t4 <- stan_lmer(score~1+attention*(self_efficacy + gender) + (1|classroom_id),data=df,
                prior_intercept=normal(0,0.5), prior=normal(0,0.5),
                chains=3,cores=3,seed=666)

#compare them via leave-one-out (loo) cross validation
loo_compare(loo(t0),loo(t1), loo(t2), loo(t3), loo(t4)) #yes! In this simple example.

#bayesian model stacking (averaging)
#use leave one out (loo) cross validation to estimate the predictive power of each model (ELPD)
loo1 <- loo(t1)
loo2 <- loo(t2)
loo3 <- loo(t3)
loo4 <- loo(t4)

loo_list <- list(loo1,loo2,loo3,loo4)

#extract the model weights from the list
wts <- loo_model_weights(loo_list)

wts

### now, let's "stack" the models and look at the "effect" of a one unit difference in X on Y

nd <- df
nd$attention[] <- 0
pp1 <- posterior_predict(t1, newdata=nd, re.form=~0)
pp2 <- posterior_predict(t2, newdata=nd, re.form=~0)
pp3 <- posterior_predict(t3, newdata=nd, re.form=~0)
pp4 <- posterior_predict(t4, newdata=nd, re.form=~0)


avg_pp0 <- wts[1]*pp1 + wts[2]*pp2 + wts[3]*pp3 + wts[4]*pp4

nd$attention[] <- 1
pp5 <- posterior_predict(t1, newdata=nd, re.form=~0)
pp6 <- posterior_predict(t2, newdata=nd, re.form=~0)
pp7 <- posterior_predict(t3, newdata=nd, re.form=~0)
pp8 <- posterior_predict(t4, newdata=nd, re.form=~0)

avg_pp <- wts[1]*pp5 + wts[2]*pp6 + wts[3]*pp7 + wts[4]*pp8

#model stacked
mean(avg_pp-avg_pp0)
sd(avg_pp-avg_pp0)

#proportion of posterior draws > 0

mean((avg_pp-avg_pp0)>0) #56% of posterior > 0, %56 confident in direction of effect

# Calculate the ATE
ate <- avg_pp - avg_pp0

# Create a data frame
ate_data <- data.frame(ATE = ate)

# Combine all ATE columns into a single vector
ate_vector <- as.vector(as.matrix(ate_data))

# Create a new data frame with this single vector
ate_df <- data.frame(ATE = ate_vector)

# Plot histogram using ggplot
ggplot(ate_df, aes(x = ATE)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(ATE)), color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(ate_vector) + sd(ate_vector), y = Inf, label = sprintf("Mean = %.2f\nSD = %.2f\nP(D>0) ~ %.2f", 
                                                                                   mean(ate_vector), sd(ate_vector), mean(ate_vector > 0)), 
           vjust = 1.5, hjust = -0.5, size = 5) +
  labs(title = "Histogram of Estimated Effect of Attention (BMS)",
       x = "Estimated Effect of Attention",
       y = "Frequency") +
  theme_minimal()

### now let's plot the posterior predicted values across a range of attention to visualze the modeled relationship

# Calculate the mean of the variables
mean_gender <- mean(df$gender, na.rm = TRUE)
mean_self_efficacy <- mean(df$self_efficacy, na.rm = TRUE)

# Assuming you have a sequence of points for prediction
ATT_SEQ <- seq(from =-2.5, to = 2.5, by = 0.5)

# Prepare data frames for each cohort group
nd_1 <- data.frame(
  gender=rep(mean_gender,length(ATT_SEQ)),
  self_efficacy=rep(mean_self_efficacy,length(ATT_SEQ)),
  attention = ATT_SEQ)


y_1 <- posterior_predict(t1, newdata = nd_1, re.form = ~0)

y_2 <- posterior_predict(t2, newdata = nd_1, re.form = ~0)

y_3 <- posterior_predict(t3, newdata = nd_1, re.form = ~0)

y_4 <- posterior_predict(t4, newdata = nd_1, re.form = ~0)


y_bms <- wts[1]*y_1 + wts[2]*y_2 + wts[3]*y_3


library(ggplot2)

# Convert predictions to a data frame for plotting
df_bms <- data.frame(attention = rep(ATT_SEQ, each = nrow(y_bms)), predicted = as.vector(y_bms))

##########

# Prep raw data for plotting
df_plot <- df

# Prepare the predicted data frame again if needed
n_draws <- nrow(y_bms)  # Assuming each row is a draw
df_bms <- data.frame(
  attention = rep(ATT_SEQ, each = n_draws),
  predicted = as.vector(y_bms))  

# Plotting
boxplot(predicted ~ attention, data = df_bms, axes = FALSE, outline = FALSE, ylim = c(-3, 3),
        xlab = "Attention", ylab = "Posterior Predicted Score", main = "Posterior Prediction", col = "lightblue")

# Set x-axis
axis(1, at = 1:length(ATT_SEQ), labels = format(ATT_SEQ, nsmall = 1), las = 2)  # Ensure labels are formatted nicely

# Set y-axis
axis(2, las = 1)

# Overlay raw data with jitter
# Scale the attention values from raw data to match the plotting scale
scaled_attention <- scale(df_plot$attention, center = min(ATT_SEQ), scale = max(ATT_SEQ) - min(ATT_SEQ))
jittered_positions <- jitter(scaled_attention * length(ATT_SEQ))
points(jittered_positions, df_plot$score, col = "darkblue", pch = 19, cex = 0.5)
