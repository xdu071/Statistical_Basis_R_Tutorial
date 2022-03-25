# Notes for tutorial 

Many coding club tutorials offers "easy to understand" introductions to sophistocated methods of data inferences and visualizations.  However, many times when a student asks  "why this works" or "how do we come to this decision", it becomes a cumbersome process for demonstrators to outline the entire statistical and mathematical background without using generalized terms.  Having many of these "awe-struck" moments myself, I often resorted to variety of sources to seek answers from simple mathmeatical regressions off Investopedia to looking at how P-value is developed through basic Cousera statistical courses.  I decided for my final project in Coding Club, I wish to summarize some these knowledge that I have learned and offer a introduction to "Statistical Inference", often considered the backbone of data science, to non-statistics or mathematical students through simple data visualizations of ecological data sets.

In developing this tutorial, I have also developed fluency in the Latex formulations to outline mathematical formulas.  Below are the notes behind some of the content I will include in this tutorial.  For anyone reading, feel free to comment your thoughts or correct me if I have made a mistake in understanding some of these concepts.  Since this tutorial will be aimed for those seeking for fundamental statistical basis for data science, it is important that we introduce accurate information using well developed examples

## 1. Why do we take samples?

### 1.1. Population, samples, and sample statistics

Statistics is all about inferring from samples drawn from a large population.  We can draw many samples from a population and in each sample, we can summarize the data points by central tendency, spread, and shape of the distribution.  These three are sample statistics because they say something only about the specific sample that we have collected.

It is important to distinguish the difference between **sample distribution** and **sampling distribution**.

**Sample Distribution** - distribution of data points collected within a single sample drawn from a population

**Sampling Distribution** - distribution of sample statistics computed from all the samples drawn from a population.  This is often used in meta-analysis to draw generalized large scale conclusion about specific systems we are interested in studying.


### 1.2. Central Limit Theorem (CLT)

As sample size gets larger, sampling distribution will resemble a normal distribution.

**Conditions**
1. Independence of data points 
    - Individuals are sampled randomly
    - Individual are sampled without replacement, a rule of thumb is that samples must be less than 10% of the population.

2. Sample distribution must not be skewed or sample size must be large enough.  A rule a them is that n > 30.

### 1.3. Normal Distribution

This is a very strict distribution where variability are highly specific around the mean.  As a result, distributions can only be "nearly normal".

*Note: Can demonstrate with a generated normal distribution and a normalized (z score) distribution of an ecological datasets*
- 68% 1 s.d. away from mean
- 95% 2 s.d. away from mean
- 99.7% 3 s.d. away from mean

**z score**

z = (obs - mean) / s.d.



## 2. Confidence Interval (CI)

Confidence intervals (CI) are plausible range f values of a population mean.  A central piece of making CI is the margin of error.

### 2.1. Margin of Error (ME)

Standard Error (SE) = s.d. / sqrt(n)

ME = 2 SE

### 2.2. Confidence Level (CL)

If we were to take many samples and build confidence interval from each sample, then about 95% (a CL) of hese intervals would contain the true population mean.

*Note: can illustrate with a chart with dot and lines in between signifying confidence intervals*

Reporting confidence interval:

sample mean + or - z-score of CL * (SE)



## 3. Hypothesis Testing

In hypothesis testing, we are testing our expected value against a status quo, aka the null value

### 3.1. P-value

The probability of observed value given that the null hypothesis is true.  To interpret the p-value, it is the likelihood of observed value or **more extreme** (refering to greater than or less than the observed value) when the null hypothesis is true (meaning the population mean is assumed to be null which in the case of standardized z-score is 0).

### 3.2. Decision Errors

Type I Error - Reject null when null is true.  It's likelihood is represented by the critical value **alpha**.
Type II Error - Fail to reject null when alternative is true.


## 4. t-distribution 

The normal distribution has absolute requirements for variability around the mean.  This is often never the case when looking at real world data.  Instead, the t-distribution is more leniet with variability.  

Looking at the t-distribution, it is basically the normal distribution of the bell shape curve but with thicker tails.  This suggests that obsevations are more likely to fall 2 s.d. away from the mean compared to the normal distribution.  This further mitigates the effect of a less reliable estimate for standard error of sampling distribution.

### 4.1. Degree of freedom (df)

The new parameter of t-distribution.  As DF increases, the t-distritbution becomes more normal.

DF = n - 1, suggests the more samples we collect, we are more likely going to get a normal distribution.

### 4.2. CI of t-distribution

Same as normal but with t-statistics.

x + or - t (given df) * (s.d / sqrt(n))

The conditions are the exact same as normal distribution.



## 5. Inferences for comparing 2 means

Same thing but the thing we are inferencing for is the difference between 2 means.  Naturally, null would be n difference between 2 mean and alternative is not 0.

## 6. Linking all this back to ANOVA, like 5 but comapring between means of multiple groups, variation considered by every combination





