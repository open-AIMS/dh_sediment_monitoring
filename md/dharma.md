Since statistical models are low dimensional representations of a
system the reliability of a statistical model will depend on the
degree to which certain assumptions are met. Many of these assumptions
can be explored by examination of the model residuals. 

Patterns in residuals suggest either issues of dependencies (biases),
poor model structure and a general lack of model fit. However, for
many statistical models, discerning genuinely violation-indicating
patterns in residuals from artifacts due to the model type can be
really difficult - if not impossible.  

Within R, the _DHARMa_ (Diagnostics for HierArchical Regression Models: @DHARMa)
package generates standardised residuals via simulation and uses these
as the basis of a range of tools to diagnose common modelling issues
including outliers, heterogeneity, over-dispersion, autocorrelation.

New observations simulated from the fitted model are used to calculate
a cumulative density function (CDF) that describes the probability
profile of each observation. Thereafter, the residual of an
observation is calculated as the value of the CDF that corresponds to
the actual observed value:

- a value of 0 indicates that the observed value was less than all
  simulated values
- a value of 1 indicates that the observed value was greater than all
  simulated values
- a value of 0.5 indicates that the probability of obtaining the
  observed value is 50%.

This approach ensures that all residuals have the same interpretation
irrespective of the model and distribution selected.

_DHARMa_ supports a variety of diagnostic plots based on the simulated residuals, including residual plots, QQ plots, and test-based diagnostic plots. These plots allow for visual inspection of model adequacy and can reveal patterns or deviations indicative of model misspecification or violation of assumptions.  The most common plots and their interpretations are:

- a **Q-Q plot** 

    Ideally all points should be close to the diagonal red line.  Overlayed onto this plot are three additional tests.
    
    1. KS (Kolmogorov-Smirnov) test investigates whether the (in this
       case simulated) are likely to have been drawn from the
       nominated distribution.
    2. Dispersion test investigates whether the is any evidence of
       overdispersion (more variability than the model expects)
       estimated as the standard deviation of the data is equal to
       that of the simulated data)
    3. Outlier test investigates for the prevalence of outliers (when
       observed values are outside the simulated range)
 
- a **Residual vs Predicted plot** 
  
  Ideally, there should be no patterns in the residuals. To help
  identify any patterns, quantile trends are overlayed. Ideally, there
  should be a flat black line at each of the quantiles of 0.25, 0.5
  and 0.75. In some circumstances, quantiles cannot be computed and in
  such cases a single dashed smoother many be placed over the data
  cloud.
  
- a **Dispersion plot**

  The observed model dispersion is overlayed (red line) upon the
  distribution (black) of simulated dispersion values. Ideally the red
  line should be in the middle of the simulated distribution.
  
  - If the red line is to the far right, the model is considered
    overdispersed. Parameter uncertainty is typically underestimated
    in overdispersed models - this leads us to be more confident in
    our results than we should be and this is bad.
  - If the red line is to the far left, the model is considered
    underdispersed. This is usually an artifact and results in our
    estimates being more conservative than they perhaps should be.
    Underdispersion is less of an issue as it just results in more
    conservatism in results.

**Note**, the individual tests that accompany the diagnostic plots
tend to be stricter than the assumptions we are seeking to explore.
That is, statistical models tend to be reasonably robust to mild
assumption violations, yet the diagnostic tests are fairly strict.
Hence, the tests are used to flag potential issues, yet the ownace is
still on the researchers to explore these violations in greater detail
and evaluate whether they are likely to have any important
consequences.

