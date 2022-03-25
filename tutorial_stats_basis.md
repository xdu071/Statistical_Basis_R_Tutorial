<center><img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/title.png" alt="Img"></center>

### Tutorial Objectives:

#### <a href="#section1"> 1. Describing Data </a>

#### <a href="#section2"> 2. Taking Samples</a>

#### <a href="#section3"> 3. Confidence Intervals</a>

#### <a href="#section4"> 4. Hypothesis Testing</a>

#### <a href="#section5"> 5. T-Tests </a>

#### <a href="#section6"> 6. Inference for 2 Independent Means </a>

#### <a href="#section7"> 7. Moving Forward... </a>

---------------------------
Many people wanting to study data science ask **how much statistics do I really need?**  Well, the answer isn't a simple one because it is literally **as much as you need it** to complete your projects of desire.  As we progress through data science, we often have to pick up new knowledge on the go whether that would be new statistical methods, graphical illustrates, or even new programming languages.  While adaptivity and proactive learning is key, there are some basic concepts of statistics that would help build your confidence in tackling tougher problems.

One of these fundamental concepts is **statistical inference**.  In statistics and much of data science, we often have to use limited information to describe large-scale and complex situations.  This demands solid understanding on degree of generalization, uncertainties around estimates, and making decisions under partial information.  This tutorial will walk you through fundamental statistical knowledge from describing data, sampling, to building confidence intervals and statistical tests using data visualization techniques in RStudio.  

You can get all of the resources for this tutorial from <a href="https://github.com/EdDataScienceEES/tutorial-xdu071" target="_blank">this GitHub repository</a>. Clone and download the repo as a zip file, then unzip it.

## Getting started

Do not worry if you have never used RStudio before.  If this is your first time, check out the Coding Club tutorial <a href="https://ourcodingclub.github.io/tutorials/intro-to-r/" target="_blank">Getting Started with R and RStudio</a>.  It should be noted that this tutorial is intended for people with prior knowledge of data manipulation with `dplyr` and visualization with `ggplot2` but wanted gain a more solid statistical background.  If at any point of the tutorial, you feel overwhelmed by the code, feel free to refer to the other coding club tutorials.  I personally recommend   <a href="https://ourcodingclub.github.io/tutorials/data-manip-efficient/" target="_blank">Efficient Data Manipulation</a> and  <a href="https://ourcodingclub.github.io/tutorials/datavis/" target="_blank">Beautiful and Informative Data Visualization</a>.

Moving onward, the data we will be using for this tutorial is <a href="https://data.nal.usda.gov/dataset/tree-height-and-diameter-data-aspen-face-experiment-1997-2008" target="_blank">Aspen FACE Experiment</a>. from the US Department of Agriculture.  The dataset investigates development of aspen saplings under various concentrations of carbon dioxide and ozone.  

After you have downloaded the repository, open up `Rstudio` and create a blank script `aspen.R`.  This is where we will be doing our coding.

Before we start, let's download the following packages.

```r
## Libraries ------

# Install packages
devtools::install_github('alanarnholt/BSDA')  # very useful package for learning basic statistics
install.packages('statsr') # containing short cuts to conducting statistical inferences
install.packages('tidyverse') # contains ggplot2 and dplyr

# Load Libraries
library(tidyverse)  
library(statsr)
library(BSDA)
```

You probably have never seen `BSDA` and `statsr`.  Both are customized packages that contains useful functions for statistical inference.  `BSDA` is developed by Alan T. Arnholt and contains shortcut functions for conducting statistical inference.  Similarly `statsr` is contains useful tools to visualize confidence intervals.  For more information, please refer to  https://cran.r-project.org/web/packages/BSDA/index.html and https://cran.r-project.org/web/packages/statsr/index.html respectively.

I would also like you to use `theme_publish` by running the following code.
```r
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
```

This package is extremely useful for basic visualization and is often used by ecologists for publishing visualization results.  To learn more about making your custom ``ggplot2`` themes, I recommend the following tutorial, <a href="https://ourcodingclub.github.io/tutorials/data-vis-2/" target="_blank">Data Visualization 2</a>.

Now let's get down to business.

<a name="section1"></a>

## 1. Describing Data

First, let's load up the `aspen.csv` and explore the data class of its variables.

```r
# Load Data ----
aspen <- read.csv("raw_data/aspen.csv", header = TRUE)
str()
```

The dataset contains 1225 aspen trees.  Trees are assigned to 4 different treatments `Treat` (1 = control, 2 = elevated levels of CO2, 3 = elevated levels of O3, 4 = combination of CO2 and O3).

`sap_final_height` is the recorded height of the sapling after 3 years of planting the initial sapling.  This will be the variable of interest as during the experiment, scientists hypothesized that CO2 and O3 influence the development of saplings.

When describing data there are 3 key parameters to considered: **shape**, **center**, **spread**.  Let's look at center and spread first.

Center is also known as the **central tendency** and these are commonly represented by **mean**, the arithmetic average of a sequence of numbers, and **median**, the middle value when ranking data in terms of numerical value from smallest to largest.

```r
aspen %>% summarize(mean = mean(sap_final_height), median = median(sap_final_height))
```

Spread in the case of statistical inference is most commonly summarized by **standard deviation**, which summarizes how much individual data points differ from the mean.

```r
aspen %>% summarize(s.d. = sd(sap_final_height))
```

There are many ways of visualizing the shape of a data distribution.  **Histograms** are one of the most useful methods as it not only illustrates the shape of the distribution but also its relation to central tendency and spread.  

```r
ggplot(data = aspen, aes(x = sap_final_height)) +
        geom_histogram(bins = 20, color = "white", fill = "slategray2") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()
```

It appears the distribution of sapling heights have a single peak between 50 and 75.  In a way, we can say our data is somewhat resembles a **normal distribution**. We can plot our mean (solid line) and median (dotted line) as a way of illustrating this.

```r
ggplot(data = aspen, aes(x = sap_final_height)) +
        geom_histogram(bins = 20, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(aspen$sap_final_height),  # add verticle line at mean
                   colour = "darkgray", size = 0.8) +
        geom_vline(xintercept = median(aspen$sap_final_height), linetype = 2,  # add median
                   colour = "darkgray", size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/approx_norm.png" alt="Img" style="width: 800px;"/> </center>


We can see that the mean and median almost overlap.  If the distribution were to be skewed left or right, this would not be the case.  To illustrate this, let's look at the distribution of the final tree height (`tree_final_height`) of these samplings and their resulting mean and median.

```r
ggplot(data = aspen, aes(x = tree_final_height)) +  
        geom_histogram(bins = 20, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(aspen$tree_final_height),
                   colour = "darkgray", size = 0.8) +
        geom_vline(xintercept = median(aspen$tree_final_height), linetype = 2,
                   colour = "darkgray", size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/skew.png" alt="Img" style="width: 800px;"/> </center>

We can see the distribution is left skewed.  The mean is clearly smaller than the median, which mean sense in a left skewed distribution since mean measures are more affected by extreme values compared to median.


<a name="section2"></a>

## 2. Taking Samples

As we said previously, statistics is all about using limited information to approximate a larger system.  We refer to this "larger system" we want to understand as the **population** and the values describing it as **population parameters**.  We take representative **samples** from the populations and compute **sample statistics** that attempts to estimate the population parameters.

Let's assume that the population we are making inference on is our dataset and the previous mean we calculated for `sap_final_height` is a population parameter `mu`.  We can compute the following that will be useful later.

```r
# Create values for parameters
params <- aspen %>%
        summarize(mu = mean(sap_final_height), sigma = median(sap_final_height))
```

### 2.1. Sample v. sampling distribution

**Sample distributions** are the distribution of observations within a sample.  If we were to take multiple samples of the same size and compute sample statistics for each, then the distribution of these sample statistics is the  **Sampling distributions**.

Using `dplyr` function `sample_n`, we can randomly sample from a dataframe a stated sample size without replacement.  But before we take samples, we need to set a seed using `set.seed()` function to ensure reproducibility.

Let's randomly sample 30 aspen saplings and compute their sample statistics.

```r
# Take a sample from Aspen population of with sample size 20
set.seed(889); sample <- sample_n(aspen, 30)
sample %>% summarize(x = mean(sap_final_height), sd = sd(sap_final_height))
```

Let's also visualize this on a histogram with mean represented as a verticle line.

```r
ggplot(data = sample, aes(x = sap_final_height)) +  
        geom_histogram(bins = 8, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(sample$sap_final_height),
                   colour = "darkgray", size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()
```
<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/sample_dist.png" alt="Img" style="width: 800px;"/> </center>

What we have constructed here is a single sample distribution of the aspen samplings.  It appears our sample distribution is centered around the sample mean of 57 and our sample distribution appears to be approximately normal.

Now let's try and create a sampling distribution from our dataset.  This part is a bit tricky especially if you are not familiar with loops.  What we need to do is first create an empty data.frame with number of rows equal to the number of samples we want to conduct.  Let's take 20 samples for now.

```r
# Create an empty vector of length of the number of samples we want to have
sample_dis <- data.frame(sample = c(1:20),
                           sample_mean = c(1:20))
```

Then we populate the data.frame with sample means. Remember to always set the seed before doing any randomized simulations.  Here, I loop through our empty data.frame and each time we go through a row, we populate it with sample mean from random samples we take without replacement from `aspen`.  In sample, we randomly sample 30 individuals.

```r
# Fill in the empty vector with sample means
set.seed(187299)
for(i in 1:nrow(sample_dis)){
        sample_dis[i, 2] = mean(sample_n(aspen, 30)$sap_final_height)
}
```

Let's look at the sampling distribution and its mean and standard deviation.

```r
# Look at sampling distribution
ggplot(data = sample_dis, aes(x = sample_mean)) +  
        geom_histogram(bins = 6, color = "white", fill = "slategray2") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()

# Compute summary statistics of sampling distirbution
sample_dis %>%
        summarize(x = mean(sample_mean), sd = sd(sample_mean))
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/sampling_dist.png" alt="Img" style="width: 800px;"/> </center>

Again it appears to be nearly normally distributed.  Looking at the mean of the sampling distribution, it seems extremely similar t the overall mean of our entire data set.  This brings us to the next topic I want to introduce, which remains central to statistical inference.

### 2.2. Central Limit Theorem

The **central limit theorem (CLT)** states that the distribution of sample statistics is nearly normal, with mean equal to the population mean, and with a standard deviation equal
to the population standard deviation divided by square root of the sample size.

Let's test to see if this is true.  Recall we previously calculated the population parameters and saved it in an object called `params`.

```r
params
sample_dis %>%
        summarize(x = mean(sample_mean), sd = sd(sample_mean))
```

This definitely seems to be the case for mean of the sampling distribution but standard deviation seems a bit off.  

There are 2 conditions for CLT.

1. Sampled observations must be **independent**.  This means they must be randomly sampled and are sampled without replacement.  A rule of thumb is that the sample size must not exceed 10% of the population.

2. Either the population distribution must be normal or if the population distribution of skew, the sample size must be large enough.  A rule of thumb is that the sample size must at least be 30.

By satisfying these two conditions, this means we can use our sample statistics to approximate the population parameters.  Before we move onto inference, I would like to discuss a bit about the normal distribution.

### 2.3. Normal distribution

Normal distribution is a unimodal "bell-shaped" curve with very strict requirements of data spread around the mean.  This refers to 68% of the data must fall within 1 standard deviation from the mean, 95% fall within 2 standard deviations from mean, and 99.7% within 3 standard deviations from mean.  Using function `stats_functions()` within `ggplot2` we can draw a curve of a typical normal density distribution function generated by function `dnorm()`.

```r
(norm_dis <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
         stat_function(fun = dnorm, n = 1225, args = list(mean = 0, sd = 1), aes(color = "normal")) +  # draws a normal distribution function as a continuous curve
         labs(y = "frequency", x = "standardized-values", color = "distribution") +
         scale_y_continuous(breaks = NULL) +
         scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +
         theme_publish()) +
         scale_color_manual(values=c("#173F5F", "#20639B", "#3CAEA3", "F6D55C", "ED553B"))
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/norm_dist.png" alt="Img" style="width: 800px;"/> </center>

Realize that the center of the above distribution is at 0.  This is referred as **standardization** and there are many **test statistics** that standardize the data.  The one that is attributed to normal distribution is called the **standardized z-score** and is computed as (observation - mean) / standard deviation.

The `scale()` function automatically helps you with this computation and this becomes extremely useful in overcoming issues within linear regression.  Below we construct a histogram using standardized value of the first sample we have collected.

```r
# Standaridization using our collected sample and compare with the previous normal distribution
sample <- sample %>%
        mutate(z = scale(sap_final_height))
ggplot(data = sample, aes(x = z)) +
        geom_histogram(bins = 5, color = "white", fill = "slategray2") +
        ylab("Frequency") +
        xlab("Standardized z-score") +
        scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()  
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/sample_z.png" alt="Img" style="width: 800px;"/> </center>



<a name="section3"></a>

## 3. Confidence Intervals

**Confidence Intervals (CI)** shows a point estimate and its associated uncertainties.  The point estimate in our case refers to the sample mean and the associated uncertainties are illustrated by the arbitrary **confidence level** we quote.

### 3.1. Building a Confidence Intervals

Uncertainties around a point estimate is determined by the **margin of error** which can be interpreted as the allowable amount of uncertainty or deviations around a point estimate.  This is usually dictated by the confidence level.  Margin of error is also considered a "2-sided" standard error and therefore can be calculated by multiplying the standard error by 2.  For instance, if we were to calculate the margin of error around the mean estimate of our first sample, we would write:

```r
sample %>%
        summarize(ME = 2 * (sd(sap_final_height)/sqrt(nrow(sample))))
```

Following onto the previous definition, a CI can then be computed as:

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/CI.png" alt="Img" </center>

where x is the sample mean and z is the associated test statistics in relation to a specific percentile within a normal distribution.

We can calculate z using the `qnorm()` function in base R where it takes in a percentile below the normal distribution curve.  For instance, if we were to calculate z critical value for a 95% confidence level, we would first determine the upper and lower percentile.  Remember, 95% confidence level refers to the percentage around the mean, which refers to the middle 95%.  Therefore to get the upper and lower end, we must subtract 1 from 0.95 and divide by 2 (remember it is the middle 95%).

```r
# If we are looking for 95% confidence interval, z* is:
qnorm(0.025) # lower end, (1-0.95) / 2
qnorm(0.95+0.025) # upper end
```

It turns out our z-critical value is around 1.96.  Now, knowing the sample mean, marginal of error, and z critical value, we can construct the 95% CI of our estimate.

```r
# Present the confidence interval of our sample
sample %>%
        summarize(ci_lower = mean(sap_final_height) -
                          1.96*(sd(sap_final_height) / sqrt(30)),
                  x = mean(sap_final_height),
                  ci_upper = mean(sap_final_height) +
                          1.96*(sd(sap_final_height) / sqrt(30)))
```

### 3.2. Interpreting CI

What does a 95% CI mean?  An empirical way to interpret this is that if we were to ake many samples from our population and compute confidence intervals for each sample, about 95% of these samples will yield confidence intervals that include the true population mean (parameter).

Again, the best way to illustrate this is visually.  Say we want to randomly take 20 samples from the aspen dataset and construct a 95% CI for each sample, them 19 of these CI would have to capture the true population mean (`mu`) which is 57.72 according to `params`.

This time, we utilize a new function from `statsr` called `rep_sample_n` which allows us to take multiple samples of a specific size without constructing a complicated for loop.  Here, we want to create data.frame consisting of 20 samples each with constructed 95% CIs (upper and lower bounds).  We then want to specify if they have captured the true population mean (`mu`).

```r
# Create dataframe with 95% of each of our 20 samples
ci <- aspen %>%
        rep_sample_n(size = 30, reps = 20, replace = FALSE) %>%
        summarise(lower = mean(sap_final_height) - 1.96 * (sd(sap_final_height) / sqrt(30)),
                  upper = mean(sap_final_height) + 1.96 * (sd(sap_final_height) / sqrt(30)))

# Determine if the samples have included mu or not
ci <- ci %>%
        mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))

# Collate all figures into a common dataframe for visualization
ci_data <- data.frame(ci_id = c(1:20, 1:20),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))

```

We will then visualize this CIs by stacking them by their ID and determine if they include `mu` or not.

```r
# Plot CIs stacked on top of each other
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id,
                           group = ci_id, color = capture_mu)) +
        geom_point(size = 2) +  # add points at the ends
        geom_line() +           # connect with lines
        labs(color = "Capture population mean (mu)?") +
        geom_vline(xintercept = params$mu, color = "darkgray", linetype = 2, size = 1) +  vertical line
        scale_color_manual(values=c("#ED553B", "#20639B")) +
        theme_publish()
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/ci_demo.png" alt="Img" style="width: 800px;"/> </center>

Wow, it appears 19 our samples included the true mean, which just happens to be 95% of all samples.  Hopefully this provides a clearer idea of what we mean when we present a confidence interval of some confidence levels.

<a name="section4"></a>
## 4. Hypothesis Testing

Within a hypothesis test, we are finding statistical evidence from our observation against some pre-existing claim or status quo.  Hypothesis testing differ from inferences through confidence intervals where instead of stating our observation and estimates, we are making a decision of choosing one claim over another given observed data.

### 4.1. Null and Alternative Hypotheses

Hypothesis test involves pitting two conflicting claims against each other.  They are usually set up as follows:

**Null Hypothesis (H0)**: usually refers to previous knowledge in literature, or prior knowledge about some form of data before observation, some form of status quo.

**Alternative Hypothesis (HA)**: this is your research question and often what you are expecting to see given new pieces of scientific information or insights.

Using our collected sample as an example, say we know from prior knowledge that the average height of aspen saplings in North America is about 51.3 cm.  However, we would like to challenge that claim based on our observation in which our sample statistic computed previously yield an average height of 56.6 cm.

Therefore our H0 states aspen sapling height as 51.3 cm while our HA states aspen sapling height is greater than 51.3.

### 4.2. P-values

P-values are centerpieces in hypothesis testing as they inform us about the significance of our observed values against the null hypothesis.  P-value is defined as the probability of our observed or **more extreme** outcome assuming hat that H0 is true.  The "more extreme" refers to the fact hypothesis tests are also known as "tail tests" because they don't just consider one point estimate.

In probability terms, p-values are expressed as P(observed | null hypothesis).  We will still construct a standardized z-score but this time, it is called the **Z statistics** where we standardizing according to the null hypothesis value as opposed to observed mean.  Therefore Z is computed as (observed - null) / standard error.  

Previously constructed null and alternative hypothesis stated above, we calculate the z-test statistics.

```r
observed <- mean(sample$sap_final_height) # our sample mean
null <- 51.3  # null hypothesis based on prior information
SE <- sd(sample$sap_final_height) / sqrt(nrow(sample)) # SE computed using sample sd
(Z <- (observed - null) / SE) # Calculate the Z-test statistics
```

Our P-value can also be thought of in our case as P(observed or more extreme) which then translates to P(Z > 2.05).  Since z-scores are in respect to normal distribution, we can visualize our calculated Z as taking a position on the tail of the normal bell shaped curve.

```r
# Revisualize on a normal distribution
norm_dis +
        geom_vline(xintercept = Z, color = "darkgray", linetype = 2, size = 1)
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/norm_z_score.png" alt="Img" style="width: 800px;"/> </center>

We can see that our calculated Z is quite far on the right side of the tail.  The further right or left a Z value is positioned on the tail, the lesser the area they take up under the curve, hence the less likely it is to observe these values when the null hypothesis is true.  In our case, there is a good chance that our p-value is low enough to be significant.  Let's check it out with `z.test()` function from `BSDA`.

```r
z.test(x = sample$sap_final_height, alternative = "greater", mu = null, sigma.x = sd(sample$sap_final_height),
       conf.level = 0.95)
```

Our result showed our p-value, P(Z > 1.647) = 0.0498.  This is just below the commonly cited critical value of 0.05, which means we can reject our null hypothesis and there are substantial evidence that aspen sapling heights are greater than 51.3 cm.


<a name="section5"></a>
## 5. T-Tests

In the real world, many statisticians use the **T-Test** instead of the normal distribution for making statistical inference.  This is because, using normal distribution involves more risk of yielding significant result in the case when it really shouldn't.  T-test does not require knowledge of the population standard deviation, which is often never known upon taking samples.

### 5.1. Student t-distribution

The student's t-distribution (I don't know why it's called 'student's') is the underlying distribution of t-tests.  In comparison to normal distribution, it has wider tails and takes in just a single parameter called **degrees of freedom(df)**.

Let's first see the two distribution differ.  We will visualize this through the `stats_functions()` again.  Note we can just add the new stats_function on top of the previously composed normal distribution chart.

```r
(t_dis <- norm_dis +
         stat_function(fun = dt, n = 1225, args = list(df = 1),
                       show.legend = TRUE, aes(color = "t-df1"))) +  # draws a t distribution function as a continuous curve
         scale_color_manual(values=c("#173F5F", "#ED553B"))
```
<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/norm_t_compare.png" alt="Img" style="width: 800px;"/> </center>

**df** depends upon our sample size.  df is computed sample size subtracted by 1.  This parameter influences the shape of the distribution.  Let's see what this means by overlaying t-distributions with different dfs on top of each other.

```r
(t_df_dis <- t_dis +
        stat_function(fun = dt, n = 1225, args = list(df = 10),
                      show.legend = TRUE, aes(color = "t-df10")) +
        stat_function(fun = dt, n = 1225, args = list(df = 20),
                      show.legend = TRUE, aes(color = "t-df20")) +
        stat_function(fun = dt, n = 1225, args = list(df = 50),
                      show.legend = TRUE, aes(color = "t-df50"))) +
        scale_color_manual(values=c("#173F5F", "#ED553B", "#20639B", "#3CAEA3", "#F6D55C"))

```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/t_dist_df.png" alt="Img" style="width: 800px;"/> </center>

Interestingly it appears that as we increase our sample size, hence our df, the shape of the distribution becomes more and more similar to the normal distribution.  This also illustrates the benefits of having larger sample size where the tails become more narrower and smaller differences between null value and observed sample values can result in significant results.  Let's try testing a hypothesis using t-tests.

### 5.2. Hypothesis Testing of a single mean

Suppose we are interested in seeing if saplings grown under treatment 3, elevated levels of O3 in the air, are taller than the previous null value of 51.3 cm.  Our hypothesis are structured similar as before:

H0: mu = 51.3
H1: mu > 51.3

Same as before, we first randomly select 30 saplings from aspen grown in treatment 3, but this time we need to filter the data.

```r
# Select a random sample of 30 saplings from aspen grown through treatment 3
set.seed(1217)
sample_O3 <- aspen %>%
        filter(Treat == 3) %>%
        sample_n(size = 30)
```

We then visualize the our point estimate and the associated distribution.  We draw a vertical line representing the sample mean.

```r
# Visualize the point estimate and distribution
sample_O3 %>%
        summarize(x = mean(sap_final_height), sd = sd(sap_final_height))

ggplot(data = sample_O3, aes(x = sap_final_height)) +
        geom_histogram(bins = 8, color = "white", fill = "slategray2") +
        geom_vline(xintercept = mean(sample_O3$sap_final_height),
                   colour = "darkgray", size = 0.8) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_publish()
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/samp_O3.png" alt="Img" style="width: 800px;"/> </center>

We then conduct the t-test using function `t.test()` from `BSDA`.  

```r
t.test(x = sample_O3$sap_final_height,
       alternative = "greater",
       mu = null,
       conf.level = 0.95)
```

The p-value P(t > 0.698) = 0.2454 to be greater than the critical value of 0.05.  Hence we fail to reject the null hypothesis.

Thinking about it, what if we try to compute the first hypothesis test we did under z-test but instead with t-tests.

```r
t.test(x = sample$sap_final_height,
       alternative = "greater",
       mu = null,
       conf.level = 0.95)
```

The resulting p-value P(t > 1.647) = 0.06, which is greater than the critical value of 0.05.  Hence we fail to reject the null hypothesis.  How can this be?  For a start, it can be due to the wider tail size.  We can try to overlay the test statistic on both the normal and the student's t-distribution.

```r
t_dis +
        geom_vline(xintercept = 1.6471, color = "darkgray", linetype = 2) +
        scale_color_manual(values=c("#173F5F", "#ED553B"))
```

<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/z_test_t_test_compare.png" alt="Img" style="width: 800px;"/> </center>

Here we really see the effect of the thicker tail.  With the same test statistic, they take up different probability densities.  It is clear that there are greater frequency of values that lie right of our test statistic under t-distribution compared to normal.


<a name="section6"></a>

## 6. Inference for 2 Independent Means

With the T-tests, we can also compare against 2 independent means to see if there are significant difference between them or not.  

In the case, the hypothesis are as followed:

H0: mu1 = mu2
HA: mu1 != mu2

This suggest that test comparing means are usually always 2 tailed

Let's say we are interested in the question **do Aspen saplings height differ between elevated levels of O3 compared to normal conditions?**  Saplings grown under normal conditions are in treatment 1 while saplings grown under elevated levels of O3 are in treatment 3.

Since we have already sampled from treatment 3 previously, we only need to randomly sample from treatment 1.  Before we jump in, it is important to stress that under this test, observations must be independent within groups and between groups.  The groups must not be associated with each other, hence **non-paired**.  We can assume this independence since you cannot have 2 treatment groups grown in the same chamber when you are manipulating gas concentrations in air.

Let's ago ahead and collected 30 random samples from treatment 1.

```r
# Sample from control treatment and observe the distribution
set.seed(188)
sample_c <- aspen %>%
        filter(Treat == 1) %>%
        sample_n(size = 30)

# Plot of sample distribution of control group
(control_sample <- ggplot(data = sample_c, aes(x = sap_final_height)) +
				geom_histogram(bins = 8, color = "white", fill = "slategray2") +
				geom_vline(xintercept = mean(sample_c$sap_final_height),
				        colour = "darkgray", linetype = 1, size = 0.8) +
			  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
				theme_publish())
```
<center> <img src="https://github.com/EdDataScienceEES/tutorial-xdu071/blob/master/figures/control_samp.png" alt="Img" style="width: 800px;"/> </center>

Compared to the previous figure of sample distribution from treatment 3, it does seem the sample means are quite close to each other.  Let's compute the t-test to

```r
t.test(x = sample_c$sap_final_height, y = sample_O3$sap_final_height,
       mu = 0, conf.level = 0.95)
```

We see our p-value is 0.9094, which is greater than the critical cutoff value of 0.05, hence we fail to reject our null hypothesis and conclude there is no significant different between sapling height of aspen grown in elevated O3 conditions compared to normal conditions.


<a name="section7"></a>

## 7. Moving Forward

The type of research question that can be conducted from inferences of a single mean or of the difference type two means is limited.  Many investigates within environmental sciences often involve looking at a single variable or measurement being influencing by more than 2 factors.

Looking at our `aspen` dataset, you might have wondered if tree growth from sampling stage to mature tree stage differs between trees grown in different treatments.  Because there are 4 treatments, we can't only consider the different between 2 groupings but we need to consider the overall variance and the sources of these variance.

Does this type of question sound familiar?  Yes, this would involve an **Analysis of Variance (ANOVA)** which utilizes a different test statistic, the **F-statistic**.  However, the decision making process and conditions are still the same.  We will still be making decision based on the p-value, which is still defined as the probability of our observed test-statistic occurring given certain null conditions.

However, this is beyond the scope of this tutorial.  If you are interested, I would encourage you to move on to the Coding Club tutorial <a href="https://ourcodingclub.github.io/tutorials/anova/" target="_blank">ANOVA from A to (XY)Z</a>.

Thank you for sticking along on this journey to review core statistical concepts behind the basic things we do in data science.  In this tutorial we have learned:

#### 1. Describing data distributions and how shape, central tendency, and spread interact with each other.
#### 2. The purpose of sampling from a larger population and making inferences
#### 3. Graphically illustrating confidence intervals, p-values, distribution of test statistics.
#### 4. Fundamentals behind statistical inference and statistical Tests

I hope you have become more confident to progress through your data science journey!


<hr>
<hr>

#### Check out our <a href="https://ourcodingclub.github.io/links/" target="_blank">Useful links</a> page where you can find loads of guides and cheatsheets.

#### If you have any questions about completing this tutorial, please contact us on ourcodingclub@gmail.com

#### <a href="INSERT_SURVEY_LINK" target="_blank">We would love to hear your feedback on the tutorial, whether you did it in the classroom or online!</a>

<ul class="social-icons">
	<li>
		<h3>
			<a href="https://twitter.com/our_codingclub" target="_blank">&nbsp;Follow our coding adventures on Twitter! <i class="fa fa-twitter"></i></a>
		</h3>
	</li>
</ul>

### &nbsp;&nbsp;Subscribe to our mailing list:
<div class="container">
	<div class="block">
        <!-- subscribe form start -->
		<div class="form-group">
			<form action="https://getsimpleform.com/messages?form_api_token=de1ba2f2f947822946fb6e835437ec78" method="post">
			<div class="form-group">
				<input type='text' class="form-control" name='Email' placeholder="Email" required/>
			</div>
			<div>
                        	<button class="btn btn-default" type='submit'>Subscribe</button>
                    	</div>
                	</form>
		</div>
	</div>
</div>
