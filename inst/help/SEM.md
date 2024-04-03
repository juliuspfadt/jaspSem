Structural Equation Modeling
==========================

Perform structural equation modeling (SEM) using `lavaan` (Rosseel, 2012). Go to lavaan.org for tutorials on the model syntax. See also Kline (2015).
For additional reading, see https://osf.io/xkg3j/ for an introduction to SEM in JASP, by Burger & Tanis.

## Input

### Model syntax box
-------
Here the syntax for one or more models (click the green plus sign) is to be specified. For help on how to write a model in lavaan syntax see https://lavaan.ugent.be/tutorial/

#### Data
Whether or not the input is a raw data file or a variance-covariance matrix. Defaults to raw data. If a covariance matrix is used, a sample size has to be specified. 

#### Sampling weights
Sampling weights to be used. Need to be specified in an extra variable

### Output options
-------
- Additional fit measures: Table with lots of additional fit measures.
- R-squared: Table with R-squared for all dependent variables.
- Observed covariances: Table with covariance matrix of observed variables
- Implied covariances: Table with model implied covariance matrix
- Residual covariances: Table with residual covariance matrix
- Standardized residuals: Table with standardized residuals covariance matrix
- Mardia's coefficient: Table with checks for multivariate normality
- Standardized estimates: Add standardized estimates to the parameter estimates tables
- Path diagram: Plot the model
  - Show parameter estimates: Add the estimates to the plot
  - Show legend: Show legend for abbreviations
- Modification indices: Table with the modification indices
  - Hide low indices: Whether or not to hide low indices
    - Threshold: Default 10. Hide modindices smaller than 10

### Model options
-------
- Factor scaling: How should the latent variables be scaled? What unit do we give the factors?
  - Factor loadings: default. Fix the first loading of each factor to 1. Unit of the factor is then the same as the first loading. 
  - Factor variance: Fixes the variance of each factor to 1. Unit of the factor is in SDs. 
  - Effects coding: Average of the loadings per latent is constrained to 1.
  - None.
- Include mean structure: Include mean structure. Note that, if multiple groups are specified, meanstructure is automatically included
- Fix latent intercepts to zero: When mean structure is included, we need additional constraints on some of the means. This fixes the latent intercepts (means) to zero. 
- Fix l

### Estimation options
-------
### Multigroup SEM
-------


## Output
- Additional fit measures: Table with lots of additional fit measures.
- R-squared: Table with R-squared for all dependent variables.
- Observed covariances: Table with covariance matrix of observed variables
- Implied covariances: Table with model implied covariance matrix
- Residual covariances: Table with residual covariance matrix
- Standardized residuals: Table with standardized residuals covariance matrix
- Mardia's coefficient: Table with checks for multivariate normality
- Standardized estimates: Add standardized estimates to the parameter estimates tables
- Path diagram: Plot the model
  - Show parameter estimates: Add the estimates to the plot
  - Show legend: Show legend for abbreviations
- Modification indices: Table with the modification indices
  - Hide low indices: Whether or not to hide low indices
    - Threshold: Default 10. Hide modindices smaller than 10

## References
-------
- Kline, R. B. (2015). Principles and practice of structural equation modeling. *Guilford publications*.
- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. *Journal of Statistical Software, 48*(2), 1-36. URL www.jstatsoft.org/v48/i02/

## R Packages
---
- lavaan
- semPlot
- semTools
- stats
