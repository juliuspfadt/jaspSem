# jaspSem

JASP module for Structural Equation Modeling (SEM)

## Analyses

### Structural Equation Modeling (Classical)
Frequentist SEM using the lavaan package. Supports:
- Custom model specification using lavaan syntax
- Multiple group analysis
- Various estimators (ML, GLS, WLS, etc.)
- Bootstrap standard errors
- Model fit indices
- Path diagrams

### Bayesian Structural Equation Modeling
Bayesian SEM using the blavaan package. Features:
- Custom model specification using lavaan syntax (compatible with Classical SEM)
- MCMC sampling with Stan backend
- Posterior mean/median estimates with credible intervals
- Bayesian fit indices (DIC, WAIC, LOO)
- Configurable MCMC settings (burnin, samples, chains, thinning)
- Multiple group analysis support

#### Bayesian SEM Usage

The Bayesian SEM analysis uses the blavaan package to fit structural equation models using Bayesian estimation. The interface is similar to the Classical SEM analysis, with the following differences:

**Model Specification:**
- Uses the same lavaan syntax as Classical SEM
- Define latent variables with `=~`
- Specify regressions with `~`
- Indicate (co)variances with `~~`

**MCMC Options:**
- Burn-in: Number of initial MCMC samples to discard (default: 500)
- Samples: Number of MCMC samples to collect after burn-in (default: 1000)
- Chains: Number of independent MCMC chains to run (default: 3)
- Thinning: Keep every nth sample to reduce autocorrelation (default: 1)

**Output:**
- Model fit: Bayesian fit indices (DIC, WAIC, LOO) instead of χ² tests
- Parameter estimates: Posterior means/medians with credible intervals instead of confidence intervals
- Tables structured similarly to Classical SEM for easy comparison

**Example Model:**
```
# latent variable definitions
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4

# regressions
dem60 ~ ind60
```

**Note:**
- Bayesian estimation is computationally intensive and may take longer than frequentist methods
- Default priors are used (weakly informative)
- For faster testing, reduce the number of samples and chains

### Other Analyses
- Partial Least Squares SEM
- Mediation Analysis
- MIMIC Model
- Latent Growth Curve

## Installation

This module is included with JASP. For development:

1. Install JASP from https://jasp-stats.org
2. Install required R packages: lavaan, blavaan, semPlot, semTools, etc.

## References

- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. *Journal of Statistical Software*, 48(2), 1-36.
- Merkle, E. C., & Rosseel, Y. (2018). blavaan: Bayesian Structural Equation Models via Parameter Expansion. *Journal of Statistical Software*, 85(4), 1-30.
