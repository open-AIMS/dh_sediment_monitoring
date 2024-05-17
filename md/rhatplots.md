Rhat plots, also known as Gelman-Rubin convergence diagnostic plots,
are graphical tools used to assess the convergence of multiple chains
in a Bayesian analysis. These plots are based on the Gelman-Rubin
statistic (Rhat), which compares the variance within chains to the
variance between chains.

**What are they?**

Rhat represents a "potential scale reduction factor", and compares the
within-chain and between-chain variances of parameter estimates from
multiple MCMC chains. It's used to assess whether the chains have
mixed well and converged to the same target distribution.

Rhat plots visualise the Rhat statistic for each parameter estimated
in the Bayesian analysis. Each parameter has its own Rhat value, and
the plot typically displayed as histograms.

**How to interpret them:**

- **Rhat < 1.01:** 

  Generally indicates very good convergence, suggesting the chains have
  explored the target distribution efficiently and reached similar
  conclusions.
  
- **Rhat < 1.05:** 

  Generally indicates acceptable convergence.
    
- **Rhat > 1.05:** 

  Indicates significant convergence problems, meaning the chains
  haven't mixed well and could lead to biased estimates. It implies
  that at least one of the chains may have traversed different
  features of the parameter space compared to the other chain(s). If
  so, then there may well be other additional un-traversed features.
  It is possible that the MCMC sampler may not have fully explored all
  important regions of the parameter space.
