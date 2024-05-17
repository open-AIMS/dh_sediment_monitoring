ESS plots, or Effective Sample Size plots, are graphical tools used to
assess the effective sample size of MCMC samples obtained from
Bayesian analyses. The effective sample size quantifies the amount of
independent information contained in the MCMC samples and is crucial
for accurate estimation of posterior quantities.

**How do they work?**

Imagine your MCMC chain has length $n$, but due to autocorrelation, not
all $n$ iterations provide truly independent information. ESS attempts
to quantify this by estimating the number of independent samples
equivalent to your chain, effectively capturing the information
content. 

**How to interpret them:**

- **High ESS (closer to $n$):** 

    This indicates good efficiency, meaning your chain efficiently
    explores the parameter space and provides reliable estimates.
    
- **Low ESS (much smaller than $n$):** 

    Suggests poor efficiency, with high autocorrelation reducing the
    effective number of independent samples. This can lead to wider
    credible intervals and less precise estimates.
    
In order to have enough MCMC samples to provide meaningful summaries,
each parameter should have at least 1000 effective samples.

The ESS values are often expressed as fractions of the total $n$ and
represented graphically as a histogram. Ideally, most (if not all) the
ESS values should be above 0.5.

If there are low ESS, it suggests that either the total number of
iterations was not large enough or the sampler was not sampling
efficiently. For the former case, the sampler should be re-run with
additional iterations. In the later case, it is likely that the model
itself (or the priors) are mis-specified and this should be addressed
before rerunning the model.
