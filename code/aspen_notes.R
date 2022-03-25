## aspen_notes.R

## Summary ------

# This script contains code snippets used for the tutorial "Developing Statistical
# Confidence for Data Science".  Here I utilized Aspen height and diameter dataset
# from the U.S. Department of Agriculture from 1997 to 2008 to illustrate fundamental
# concepts that surrounds statistical inference.

# Edited by David Du
# 08-12-2021

## Libraries ------

# Install packages
devtools::install_github('alanarnholt/BSDA')  # very useful package for learning basic statistics by Alan T. Arnholt
install.packages('statsr') # containing short cuts to conducting statistical tests

# Load Libraries
library(tidyverse)  # includes ggplot2, tidyr, dplyr
library(statsr) # Source: https://cran.r-project.org/web/packages/statsr/index.html
library(BSDA)

## Theme ------ (Source: https://rdrr.io/cran/envalysis/src/R/theme_publish.R)
theme_publish <- function(base_size = 12, base_family = "",
                          line_size = 0.25, ...) {
        half_line <- base_size / 2
        small_rel <- 0.8
        small_size <- small_rel * base_size
        theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
                theme(
                        rect = element_rect(fill = "transparent", colour = NA, color = NA,
                                            size = 0, linetype = 0),
                        text = element_text(family = base_family, face = "plain",
                                            colour = "black", size = base_size, hjust = 0.5,
                                            vjust = 0.5, angle = 0, lineheight = 0.9,
                                            margin = ggplot2::margin(), debug = F),
                        
                        axis.text = element_text(size = small_size),
                        axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4),
                                                   vjust = 1),
                        axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4), 
                                                   hjust = 1),
                        axis.title.x = element_text(margin = ggplot2::margin(t = small_size,
                                                                             b = small_size)),
                        axis.title.y = element_text(angle = 90,
                                                    margin = ggplot2::margin(r = small_size,
                                                                             l = small_size/4)),
                        axis.ticks = element_line(colour = "black", size = line_size),
                        axis.ticks.length = unit(0.25, 'lines'),
                        
                        axis.line = element_line(colour = "black", size = line_size),
                        axis.line.x = element_line(colour = "black", size = line_size), 
                        axis.line.y = element_line(colour = "black", size = line_size), 
                        
                        legend.spacing = unit(base_size/4, "pt"),
                        legend.key = element_blank(),
                        legend.key.size = unit(1 * base_size, "pt"),
                        legend.key.width = unit(1.5 * base_size, 'pt'),
                        legend.text = element_text(size = rel(small_rel)),
                        legend.title = element_text(size = rel(small_rel), face = 'bold'),
                        legend.position = 'bottom',
                        legend.box = 'horizontal',
                        
                        panel.spacing = unit(1, "lines"),
                        panel.background = element_blank(),
                        panel.border = element_blank(), 
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        
                        strip.text = element_text(size = base_size),
                        strip.background = element_rect(fill = NA, colour = "black", size = 0.125),
                        strip.text.x = element_text(face = 'bold', hjust = 0,
                                                    margin = ggplot2::margin(b = small_size/2,
                                                                             t = small_size/4)),
                        strip.text.y = element_text(angle = -90, face = 'bold',
                                                    margin = ggplot2::margin(l = small_size/2,
                                                                             r = small_size/4)),
                        
                        plot.margin = unit(c(5,5,0,0), "pt"),
                        plot.background = element_blank(),
                        plot.title = element_text(face = "bold", size = 1.2 * base_size, 
                                                  margin = ggplot2::margin(b = half_line),
                                                  hjust = 0)
                )
}

# Load Data ----
aspen <- read.csv("raw_data/aspen.csv", header = TRUE)
aspen$Treat <- as.factor(aspen$Treat)
str(aspen)

## 1. Taking Samples ----

## Summary statistics of entire dataset ----
aspen$Treat <- as.factor(aspen$Treat)

# Mean and standard deviation
aspen %>% 
        group_by(Treat) %>%
        summarize(mu = mean(sap_final_height), 
                  sigma = sd(sap_final_height)) %>%
        ungroup()

# Median and IQR
aspen %>% 
        group_by(Treat) %>%
        summarize(M = median(sap_final_height),
                  iqr = IQR(sap_final_height)) %>%
        ungroup()

# Create values for parameters
params <- aspen %>%
        summarize(mu = mean(sap_final_height), sigma = sd(sap_final_height))

# Distribution of data within each treatment (Looking at shape) ------

# Distribution of sapling height, roughly normal, mean and median located similar
ggplot(data = aspen, aes(x = sap_final_height)) +
        geom_histogram(bins = 20, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(aspen$sap_final_height),
                   colour = "darkgray", size = 0.8) +
        geom_vline(xintercept = median(aspen$sap_final_height), linetype = 2,
                   colour = "darkgray", size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()

# Distribution of final tree height, can see it is skewed left, mean < median, as
# it is more affected by extreme values
ggplot(data = aspen, aes(x = tree_final_height)) +  
        geom_histogram(bins = 20, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(aspen$tree_final_height),
                   colour = "darkgray", size = 0.8) +
        geom_vline(xintercept = median(aspen$tree_final_height), linetype = 2,
                   colour = "darkgray", size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()

# Sample v. sampling distributions ----

# See how many individuals in the population
nrow(aspen)

# Taking samples

# Need to set seed for reproducibility since taking samples are random

# Take a sample from Aspen population of with sample size 20
set.seed(889); sample <- sample_n(aspen, 30)
sample %>% summarize(x = mean(sap_final_height), sd = sd(sap_final_height))

# Look at sample distribution
ggplot(data = sample, aes(x = sap_final_height)) +  
        geom_histogram(bins = 8, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(sample$sap_final_height),
                   colour = "darkgray", size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()

# Create a sampling distribution

# Create an empty vector of length of the number of samples we want to have
sample_dis <- data.frame(sample = c(1:20),
                           sample_mean = c(1:20))

# Fill in the empty vector with sample means
set.seed(187299)
for(i in 1:nrow(sample_dis)){
        sample_dis[i, 2] = mean(sample_n(aspen, 30)$sap_final_height)
}

# Have a look at our sampling distribution
head(sample_dis)

# Compute summary statistics of sampling distirbution
sample_dis %>%
        summarize(x = mean(sample_mean), sd = sd(sample_mean))

# Look at sampling distribution
ggplot(data = sample_dis, aes(x = sample_mean)) +  
        geom_histogram(bins = 6, color = "white", fill = "slategray2") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()

# Central Limit Theorem ----
# Just define how our above example relates to the Central Limit Theorem
# -> distribution of sample statistics is nearly normal, centered at the
#    population mean with standard deviation equal to the population deviation
#    divided by sqrt of sample size.

# Check if this is true

# Population parameters
aspen %>% summarize(mu = mean(sap_final_height), sigma = sd(sap_final_height))

# Sampling distribution
sample_dis %>% summarize(x = mean(sample_mean), SE = sd(sample_mean)) # kind of true, although SE is a bit off

# This means we can use normal distribution to make inferences about our chosen
# data set.

# Normal Distribution ----

# Visualize normal distribution
(norm_dis <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
         stat_function(fun = dnorm, n = 1225, args = list(mean = 0, sd = 1), aes(color = "normal")) +  # draws a normal distribution function as a continuous curve
         labs(y = "frequency", x = "standardized-values", color = "distribution") +
         scale_y_continuous(breaks = NULL) +
         scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +
         theme_publish()) +
         scale_color_manual(values=c("#173F5F", "#20639B", "#3CAEA3", "F6D55C", "ED553B")) 

# Standaridization using our collected sample and compare with the previous normal distribution
sample <- sample %>%
        mutate(z = scale(sap_final_height))
ggplot(data = sample, aes(x = z)) + 
        geom_histogram(bins = 5, color = "white", fill = "slategray2") +
        ylab("Frequency") +
        xlab("Standardized z-score") +
        scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()  # Shows distribution can only be roughly normal


## 2. Confidence Intervals ------

# Confidence Intervals illustrate a point estimate (our sample mean) and the 
# uncertainties associated around this estimate.  

# Confidence Interval is point estimate + or - z* SE

# Marginal of error -> illustrates uncertainty surrounding th point estimate ----
# Computed as 2 times the standard error 
sample %>%
        summarize(SE = sd(sap_final_height)/sqrt(nrow(sample)))

# z* is the critical value of z that dictates the cutoff points of our confidenc
# interval. 

# This can be found by finding the area under the curve of our cutoff points

# If we are looking for 95% confidence interval, z* is:
qnorm(0.025) # lower end, (1-0.95) / 2
qnorm(0.95+0.025) # upper end

# For instance, a 95% confidence interval would be mean + or - 2 times the standard error (both )

# Present the confidence interval of our sample
sample %>%
        summarize(ci_lower = mean(sap_final_height) - 
                          1.96*(sd(sap_final_height) / sqrt(30)),
                  x = mean(sap_final_height),
                  ci_upper = mean(sap_final_height) +
                          1.96*(sd(sap_final_height) / sqrt(30)))

# Defining a 95% CI ---- 
# If we were to take many samples from our population and compute confidence
# intervals for each sample, about 95% of these samples will yield confidence
# intervals that include the true population mean (parameter). 

# We construct a layered CI chart to show this.

# First let's construct a dataframe with the same 20 samples we chosen for
# constructing the sampling distribution before

set.seed(2)

ci <- aspen %>%
        rep_sample_n(size = 30, reps = 20, replace = FALSE) %>%
        summarise(lower = mean(sap_final_height) - 1.96 * (sd(sap_final_height) / sqrt(30)),
                  upper = mean(sap_final_height) + 1.96 * (sd(sap_final_height) / sqrt(30)))

ci <- ci %>%
        mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))


ci_data <- data.frame(ci_id = c(1:20, 1:20),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))

# 19 out of 20 capture the true mean, exactly 95%, nice
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
        geom_point(size = 2) +  # add points at the ends, size = 2
        geom_line() +           # connect with lines
        labs(color = "Capture population mean (mu)?") +
        geom_vline(xintercept = params$mu, color = "darkgray", linetype = 2, size = 1) + # draw vertical line 
        scale_color_manual(values=c("#ED553B", "#20639B")) +
        theme_publish() 


## 3. Hypothesis Testing ------

# Null and alternative hypothesis ----

# Null (H0): based on prior information, we know that a typical Aspen sapling after x
# day of growth is 50 height. 

# Alternative (H1): however, based on our data, we believe height of Aspen sapling is 
# greater than 51.3

# p-value ----
# Probability of observed or more extreme outcome assuming hat that H0 is true

# We still construct a standardized z-score as before but this time it is called
# Z test statistics because we are standardizing our observation according to null
# value.
observed <- mean(sample$sap_final_height) # our sample mean
null <- 51.3  # null hypothesis based on prior information
SE <- sd(sample$sap_final_height) / sqrt(nrow(sample)) # SE computed using sample sd
(Z <- (observed - null) / SE) # Calculate the Z-test statistics

# p-value here can be thought of as P(Z > 2.05) given existing distribution

# Revisualize on a normal distribution
norm_dis + 
        geom_vline(xintercept = Z, color = "darkgray", linetype = 2, size = 1) 

# Conduct Z-test (significant result)
z.test(x = sample$sap_final_height, alternative = "greater", mu = null, sigma.x = sd(sample$sap_final_height),
       conf.level = 0.95)

# Decision-errors ----

# Type I and Type II Error
# Confidence Level = allowable proportion of Type I error

# Clarification between CI and HT ----

# CI -> displays a point estimate and the uncertainties surrounding it
#       - Always symmetric about the point estimate
#       - When standardized, 0 is the point estimate
#       - No decision is made, hence no utilization of p-value
# HT -> test an alternative claim against a status-quo
#       - Does not always have to be symmetric 
#       - Standardized according to the null hypothesis, therefore distribution
#         not centered at point estimate
#       - Utilizes the p-value to make decisions


## 4. T-tests ----

# Add t_distribution with df = 1 to normal distribution for comparison
(t_dis <- norm_dis + 
         stat_function(fun = dt, n = 1225, args = list(df = 1),
                       show.legend = TRUE, aes(color = "t-df1"))) +  # draws a t distribution function as a continuous curve
         scale_color_manual(values=c("#173F5F", "#ED553B")) 

# Show the effect of df, as sample size increases, t-distribution becomes more normal
(t_df_dis <- t_dis +
        stat_function(fun = dt, n = 1225, args = list(df = 10),
                      show.legend = TRUE, aes(color = "t-df10")) +
        stat_function(fun = dt, n = 1225, args = list(df = 20),
                      show.legend = TRUE, aes(color = "t-df20")) +
        stat_function(fun = dt, n = 1225, args = list(df = 50),
                      show.legend = TRUE, aes(color = "t-df50"))) +
        scale_color_manual(values=c("#173F5F", "#ED553B", "#20639B", "#3CAEA3", "#F6D55C")) 

# Hypothesis test using t-distribution ----

# Say this time we want to see if Aspen saplings grown through Treatment-3 differs
# from a the null sampling height of 51.6

# Select a random sample of 30 saplings from aspen grown through treatment 3
set.seed(1217)
sample_O3 <- aspen %>%
        filter(Treat == 3) %>% 
        sample_n(size = 30)

# Visualize the point estimate and distribution
sample_O3 %>%
        summarize(x = mean(sap_final_height), sd = sd(sap_final_height))

ggplot(data = sample_O3, aes(x = sap_final_height)) +
        geom_histogram(bins = 8, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(sample_O3$sap_final_height),
                   colour = "darkgray", size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()

# Not Significant
t.test(x = sample_O3$sap_final_height,
       alternative = "greater",
       mu = null,
       conf.level = 0.95)

# In fact this time it is no longer significant
t.test(x = sample$sap_final_height,
       alternative = "greater",
       mu = null,
       conf.level = 0.95)

# See the effect of "thicker-tail"
t_dis + 
        geom_vline(xintercept = 1.6471, color = "darkgray", linetype = 2) +
        scale_color_manual(values=c("#173F5F", "#ED553B")) 
## 5. Comparison against 2 means ----

# Suppose we want determine if 2 means are statistically significant different
# from each other.

# Do Aspen saplings grow less under elevated levels of O3 compared to normal
# conditions?

# We will reuse the previous sample but we still need to sample from the control
# treatment

# Sample from control treatment and observe the distribution
set.seed(188)
sample_c <- aspen %>% 
        filter(Treat == 1) %>%
        sample_n(size = 30)

sample_c %>%
        summarize(x = mean(sap_final_height), sd = sd(sap_final_height))

# Plot of sample distribution of control group
(control_sample <- ggplot(data = sample_c, aes(x = sap_final_height)) +
        geom_histogram(bins = 8, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(sample_c$sap_final_height),
                   colour = "darkgray", linetype = 1, size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish())

# Conditions for comparing means ----

# Hypothesis ----
# Null = difference = 0
# Alternative = difference does not = 0

# Compute a t-test for 2 means ----
t.test(x = sample_c$sap_final_height, y = sample_O3$sap_final_height, 
       mu = 0, conf.level = 0.95)

## 6. Comparison against multiple means ----

# What if we want to compare multiple mean? 

# Do growth of Aspen differ based on the components of their surrounding gases?
anova(lm(height_growth ~ Treat, data = aspen))

# It is significant, but why, that will have to be for next time!

        