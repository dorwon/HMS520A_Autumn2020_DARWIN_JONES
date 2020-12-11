# Darwin Jones
# HMS 510
# 11-24-2020
# Assignment 5

rm(list = ls())

library(data.table)
library(ggplot2)
library(gridExtra)
library(lme4)
library(sjPlot)

# Problem 1 ---------------------------------------------------------------
# In this problem, we try to answer which study division makes more sense. 
# Use your favorite plotting package, and plot obs against exposure by study_id1 
# and study_id2. Which one do you think is better to use as a grouping id? 
# Explain your reason.


# Load the data

dt <- fread(input = "data/assignment5_data.csv")


# Plot

p1 <- ggplot(dt, aes(exposure, obs)) +
  geom_point(aes(colour = factor(study_id1)), size = 3) +
  geom_line(aes(colour = factor(study_id1), group = study_id1), size = 2.5) +
  scale_fill_discrete(palette = 'Set3')

p2 <- ggplot(dt, aes(exposure, obs)) +
  geom_point(aes(colour = factor(study_id2)), size = 3) +
  geom_line(aes(colour = factor(study_id2), group = study_id2), size = 2.5) +
  scale_fill_discrete(palette = 'Set3')

grid.arrange(p1, p2, nrow = 2) 

# compute correlation coefficients
correlations <- apply(dt,
                 function(x) {
                   cor(dt$exposure, x)
                 },
                 MARGIN = 2)

dt[,correlation_1 := cor(obs, exposure), by = study_id1]
dt[,correlation_2 := cor(obs, exposure), by = study_id2]

summary(dt$correlation_1)
summary(dt$correlation_2)

# grouping 1 makes more sense than the 2nd because the obs follow a slightly 
# more consistent logarithmic like line. for study_id2, there does not
# look like a correlation between exposure and obs. Th./e correlation coefficients
# are also higher for group1


# Problem 2 ---------------------------------------------------------------

# In this problem, we try to answer which covariates should be included 
# in the model.
# 
# - Use the `regress_group_data` function you created in last homework to 
#   compute the coefficient for `exposure` of each group (`use_intercept = FALSE`).
# - Compute the "residual" between the observation and the prediction. 
#   (`residual = obs - exposure*coef`)
# - For each group compute the correlations between the `residual` and `cov1` ,
#   `cov2` and `cov3`. Based on these correlations, which covariate you think 
#    should be in the final model?


# get functions

source(file = "HW4_functions.R")

# get coefficients

c1 <- regress_group_data(data = dt,
                         group_id = 'study_id1', 
                         obs = 'obs',
                         cov = 'exposure',
                         include_intercept = FALSE)

dt_coef <- as.data.table(c1)

# calculate the residuals

residuals <- apply(dt, 
                   function(x) {
                     x[1] - x[2] * dt_coef[study_id1 == x[6], 2]
                     }, 
                   MARGIN = 1)


# Compare the correlation between the residuals and the other covariates

# reformat residuals, add covariates and study_id labels
dt_res <- rbindlist(residuals)
setnames(dt_res, "exposure", "residual")

dt_res[, c('cov1', 'cov2', 'cov3', 'study_id1')] =
  dt[, c('cov1', 'cov2', 'cov3', 'study_id1')]


# plot 
p3 <- ggplot(dt_res, aes(residual, cov1)) +
  geom_point(aes(colour = factor(study_id1)), size = 3) +
  geom_line(aes(colour = factor(study_id1), group = study_id1), size = 1.5) +
  scale_fill_discrete(palette = 'Set3')

p4 <- ggplot(dt_res, aes(residual, cov2)) +
  geom_point(aes(colour = factor(study_id1)), size = 3) +
  geom_line(aes(colour = factor(study_id1), group = study_id1), size = 1.5) +
  scale_fill_discrete(palette = 'Set3')

p5 <- ggplot(dt_res, aes(residual, cov3)) +
  geom_point(aes(colour = factor(study_id1)), size = 3) +
  geom_line(aes(colour = factor(study_id1), group = study_id1), size = 1.5) +
  scale_fill_discrete(palette = 'Set3')

grid.arrange(p3, p4, p5, nrow = 1)

# compute correlation
cor_res <- apply(dt_res,
             function(x) {
               cor(dt_res$residual, x)
             },
             MARGIN = 2)

# cov1 has the strongest correlation so I will use it in the model.


# Problem 3 ---------------------------------------------------------------

# After we have selected the study id and covariates, we need to determine which 
# variable should differ from study to study (random effects).
# 
# - Use the `regress_group_data` function again but include `exposure` and the 
# covariate you selected to compute the coefficients across the studies.
# - Which coefficient varies a lot across studies which doesn't?

model1 <- regress_group_data(data = dt, 
                             group_id = 'study_id1', 
                             obs = 'obs', 
                             cov = c('exposure', 'cov1'),
                             include_intercept = FALSE)

# Cov1 varies less from study to study than exposure. This will be a random effect

# Problem 4 ---------------------------------------------------------------

# Now we have all the information we need. Set up a `lme4` model, to include 
# the study id, covariates and random effects information you obtained from 
# problems above. After you have the result, plot the prediction against data 
# for each study.

fit <- lmer(obs ~ cov1 + (1 + exposure|study_id1),
           data = dt)
dt$fit <- predict(fit)
dt$error <- 1.0193 / 10 # calculate standard error from model


# graph all predictions next to data

ggplot(dt, aes(cov1, fit, color = factor(study_id1))) + 
  geom_ribbon(aes(ymax = obs+error, ymin = obs-error), alpha = 0.05) +
  geom_point(aes(cov1, obs), size = 2)

# make 1 graph for each study

graphs <- vector('list', 10)

colors <- RColorBrewer::brewer.pal(name="Paired", n=10)

for(i in seq(1,10)){
  
  sub <- subset(dt, study_id1 == i)
  graphs[[i]] <- ggplot(sub, aes(cov1, fit)) + 
    geom_ribbon(aes(ymax = obs+error, ymin = obs-error), alpha = 0.05, color = colors[i]) +
    geom_point(aes(cov1, obs), size = 2, color = colors[i]) +
    ggtitle(paste0("Study #",i))
}

grid.arrange(grobs = graphs, nrow = 2)
