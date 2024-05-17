MCMC autocorrelation plots, are another essential tool for diagnosing
the convergence and mixing of your MCMC algorithm in Bayesian
statistics. 

**What are they?**

Autocorrelation plots measure the correlation between values of a
parameter at different lags (distances) within the MCMC chain. They
typically display the correlation coefficient at each lag, plotted
against the lag number. The first bar represents the correlation of
MCMC samples with themselves, and thus will always be 1 on the y-axis.

**How to interpret them:**

- **Bias:**

    The autocorrelation plot helps assess how quickly the correlation
    between samples decays as the lag increases. Ideally, samples
    collected from any process (including MCMC sampling) should all be
    independent and unbiased in order to provide unbiased estimates.
    If the autocorrelation values decay rapidly to zero as the lag
    increases, it indicates that the MCMC samples are likely to be
    independent and unbiased. Conversely, if high autocorrelation
    persists with large lags, it is possible that the MCMC samples are
    biased towards particular regions of the parameter space.
    **Ideally, the degree of autocorrelation should drop to below 0.25
    by the second bar (second lag).

- **Convergence:**

    - Rapidly decaying autocorrelation: As the lag increases, the
      correlation between values drops quickly, indicating good
      convergence. The chain efficiently moves through the parameter
      space, providing reliable and unbiased estimates.

    - High autocorrelation even at large lags: This suggests the chain
      is "stuck" in certain regions, not exploring the parameter space
      well. Estimates might be biased and unreliable. 

- **Mixing:**

    - High autocorrelation at small lags: The chain takes many
      iterations to "forget" past values, indicating slow mixing. The
      algorithm might not be efficiently exploring the parameter
      space.

    - Autocorrelation dropping to zero around medium lags: This
      suggests the chain mixes reasonably well, considering the
      natural dependence between consecutive values. 

If autocorrelation patterns exist in the MCMC samples, the MCMC
samples should be thinned to a higher degree.
