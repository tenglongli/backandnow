#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
IntegerVector findmiss(NumericVector x){
  IntegerVector y = seq(0,x.size()-1);
  LogicalVector z = is_na(x);
  return y[z];
}


// proposal for random walk for getting imputed value for delays 
// [[Rcpp::export]]
double draw(double x){
  if (x <= 0){
    return 1;
  } else {
    NumericVector k = {x-1,x+1};
    NumericVector kk = sample(k,1); 
    return kk[0];
  }
}


// function used by mapply
double nb(double x, double r, double m){
  double y = R::dnbinom_mu(x,r,m,true);
  return y;
}

// function used by mapply for missing delay imputation
double rnb(double r, double m){
  double y = rnbinom_mu(r,m);
  return y;
}


// Simple version of like, just for imputing missing values
// [[Rcpp::export]]
NumericVector impute(double r, NumericVector mean){
  NumericVector s = rep(r,mean.size());
  NumericVector missy = mapply(s,mean,rnb);
  return missy;
}

// Function to compute the mean
// [[Rcpp::export]]
NumericVector meanvec(NumericMatrix dat, NumericVector par){
  arma::mat d = as<arma::mat>(dat);
  arma::vec p = as<arma::vec>(par);
  arma::mat k = d*p;
  NumericMatrix lc = wrap(k);
  NumericVector lc1 = lc(_,0);
  NumericVector m1 = exp(lc1);
  return m1;
}

// faster version of like for large sample
// [[Rcpp::export]]
double like(NumericVector outcome, NumericMatrix dat, NumericVector par){
  NumericVector par1 = par[seq(0,par.size()-2)];
  double r = par[par.size()-1];
  NumericVector m1 = meanvec(dat,par1);
  int n = outcome.size();
  NumericVector s = rep(r,n);
  NumericVector result = mapply(outcome,s,m1,nb);
  return sum(result);
}


// Create Matrix of dummy variables
// [[Rcpp::export]]
NumericMatrix dummy(IntegerVector week, IntegerVector weekend){
  int nw = max(week);
  int nc = nw+2;
  int n = week.size();
  IntegerVector cons = rep(1,n);
  NumericMatrix cov (n,nc);
  cov(_,0) = cons;
  for (int i=1;i<nc;++i){
    if (i < nw+1) {
      cov(_,i) = ifelse(week==i,1,0);
    } else {
      cov(_,i) = weekend;
    }
  }
  return cov;
}



// [[Rcpp::export]]
NumericMatrix mcmc(NumericVector outcome, IntegerVector week, IntegerVector weekend, int iter, double sigma, int maxdelay){
  int nw = max(week);
  int nc = nw+3;
  IntegerVector missind = findmiss(outcome);
  int nmiss = missind.size();
  NumericVector miss0 = as<NumericVector>(sample(maxdelay,nmiss,true));
  outcome[missind] = miss0;
  // Output matrix of Bayesian parameter estimates
  NumericMatrix output (iter,nc);
  NumericVector par0;
  par0 = runif(nc);
  output(0,_) = par0;
  NumericVector oldpar;
  NumericVector newpar;
  double ratio;
  double decision;
  NumericMatrix cov;
  cov = dummy(week,weekend);
  NumericMatrix misscov (nmiss,cov.ncol());
  int mi;
  for (int i=0;i<nmiss;++i){
    mi = missind[i];
    misscov(i,_) = cov(mi,_);
  }
  for (int i=1;i<iter;++i) { 
    oldpar = output(i-1,_);
    for (int k=0;k<nc;++k){
      if (k<nc-1){
        newpar = clone(oldpar);
        newpar[k] = R::rnorm(oldpar[k],sigma);
        ratio = exp(like(outcome,cov,newpar) - like(outcome,cov,oldpar));
        decision = (ratio>1)?1:ratio;
        output(i,k) = (R::runif(0,1)<decision)?newpar[k]:oldpar[k];
        oldpar[k] = output(i,k);
      } else {
        newpar = clone(oldpar);
        newpar[k] = exp(R::rnorm(log(oldpar[k]),sigma));
        if (newpar[k] < 100) {
          ratio = (newpar[k]/oldpar[k])*exp(like(outcome,cov,newpar) - like(outcome,cov,oldpar));
        } else {
          ratio = 0;
        }
        decision = (ratio>1)?1:ratio;
        output(i,k) = (R::runif(0,1)<decision)?newpar[k]:oldpar[k];
        oldpar[k] = output(i,k);
      }
      }
    NumericVector par1 = oldpar[seq(0,nc-2)];
    double r1 = oldpar[nc-1];
    NumericVector mm = meanvec(misscov,par1);
    outcome[missind] = impute(r1,mm);
    }
  
  return output;
}








