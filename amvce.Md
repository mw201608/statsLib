## Description

Linear mixded model (or quantitative genetic animal model) variance component estimation using REML

## Usage

amvce(y, X, A, tolerance=1.0e-3, maxIter=100, verbose=FALSE)

## Arguments
y, a vector of numeric values of dependent variable.

X, a matrix of fixed effects.

A, a square matrix of random effect correlation structure.

tolerance, optional, a relative tolerance to test converging.

maxIter, optional, maximum number of iteration.

verbose, optional, whether to print progress.

## Details

This function implements mixed effect model fitting by REstricted Maximum Likelihood (REML) using the Expectation Maximization (EM) algorithm. For technical details of the analysis model, please refer to the reference of Yang et al 2014.

## Value

A list with elements:

coefficients, estimates of fixed effect coefficients.

apVb, estimate of variance of fixed effect coefficients.

VC, a vector of variance component estimates.

loglik, log-likelihood

## Author

Minghui Wang <m.h.wang _at_ live.com>

## Reference

Yang S, Liu Y, Jiang N, Chen J, Leach L, Luo Z and Wang M. Genome-wide eQTLs and heritability for gene expression traits in unrelated individuals. BMC Genomics 2014, 15:13

## Examples

``` 
#Create example data
set.seed(1234567)
n=200
M1=matrix(rnorm(n*300), nrow=n)
K1=cov(t(M1))
A=K1/mean(diag(K1))
covY=2*A+1*diag(n)
y=10+crossprod(chol(covY),rnorm(n))
X=matrix(rep(1, n),ncol=1)
#Fit linear mixed model
(fit2=amvce(y,X,A))

```
