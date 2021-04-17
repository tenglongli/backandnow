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



// dnb function used by mapply for likelihood
double dnb(double x, double r, double m){
  double y = R::dnbinom_mu(x,r,m,true);
  return y;
}


// pnb function used by mapply for likelihood
double pnb(int x, double r, double m){
  double y = R::pnbinom_mu(x,r,m,true,true);
  return y;
}


// rnb function used by mapply for likelihood
double rnb(double r, double p){
  double y = R::rnbinom(r,p);
  return y;
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
double like(NumericVector outcome, NumericMatrix dat, NumericVector disp, NumericVector par, int maxdelay){
  int np = dat.ncol();
  NumericVector par1 = par[seq(0,np-1)];
  NumericVector r = par[seq(np,par.size()-1)];
  NumericVector m1 = meanvec(dat,par1);
  int n = outcome.size();
  NumericVector s;
  double r1;
  if (max(disp)==1){
    r1 = as<double>(r);
    s = disp*r1;
  } else {
    r1 = r[0];
    s = rep(r1,n);
    LogicalVector select = (disp==2);
    double r2 = r[1];
    s[select] = r2;
  }
  NumericVector num = mapply(outcome,s,m1,dnb);
  IntegerVector md = rep(maxdelay,n);
  NumericVector dem = mapply(md,s,m1,pnb);
  double result = sum(num)-sum(dem);
  return result;
}


// Create Matrix of dummy variables
// [[Rcpp::export]]
NumericMatrix dummy(IntegerVector week, IntegerVector weekend){
  int nw = max(week);
  int nc = nw+1;
  int n = week.size();
  NumericMatrix cov (n,nc);
  for (int i=0;i<nc;++i){
    if (i < nw) {
      cov(_,i) = ifelse(week==(i+1),1,0);
    } else {
      cov(_,i) = weekend;
    }
  }
  return cov;
}

// function to make proportion of counts from 0 to maxdelay
NumericVector prop(NumericVector x, NumericVector onset, int maxdelay, int cd){
  LogicalVector v = (x<=maxdelay)&(onset>=cd);
  NumericVector x1 = x[v];
  int dem = x1.size();
  NumericVector p1 (maxdelay);
  for (int i=0;i<maxdelay;++i){
    p1[i] = sum(x1==(maxdelay-i));
  }
  NumericVector p = p1/dem;
  NumericVector p2 = cumsum(p);
  NumericVector result = 1-p2;
  return result;
}



// create a lambda function to compute mean of poisson dist.
// function used in getr and updater
// curve is the estimated epidemic curve (i.e., back-calculated counts & nowcasted counts)
// [[Rcpp::export]]
NumericVector lambda(NumericVector curve, NumericVector si){
  int k = si.size();
  int n = curve.size();
  NumericVector c;
  NumericVector sic;
  NumericVector result (n-1);
  for (int i=0;i<(n-1);++i){
    if (i<k) {
      c = curve[seq(0,i)];
      sic = rev(si[seq(0,i)]);
      result[i] = sum(c*sic);
    } else {
      c = curve[seq(i-k+1,i)];
      sic = rev(si);
      result[i] = sum(c*sic);
    }
  }
  return result;
}


// curve is the estimated epidemic curve (i.e., back-calculated counts & nowcasted counts)
// Compute the mean only
// [[Rcpp::export]]
NumericVector getr(NumericVector curve, NumericVector si, int size){
  int n = curve.size();
  int nr = n-size-1;
  NumericVector result (nr);
  double shape;
  double scale;
  NumericVector incid;
  NumericVector dem = lambda(curve,si);
  NumericVector dem1;
  for (int i=0;i<nr;++i){
    incid = curve[seq(i+1,i+size+1)];
    shape = sum(incid)+1;
    dem1 = dem[seq(i,i+size)];
    scale = 1/(sum(dem1)+0.2);
    result[i] = shape*scale ; 
  }
  return result;
}


// Note: cd must be earlier than the first day of nowcasting period
// [[Rcpp::export]]
List backnow(NumericVector outcome, NumericVector days, IntegerVector week, IntegerVector weekend, 
                   int iter, double sigma, int maxdelay, NumericVector si, int size, Nullable<int> cd = R_NilValue){
  NumericVector outcome1 = clone(outcome);
  int cday;
  int nw = max(week);
  int nd = max(days);
  int nc;
  int n = outcome1.size();
  int type;
  NumericVector dpind = rep(1.0,n);
  if (cd.isNotNull()){
    cday = as<int>(cd);
    nc = nw+3;
    LogicalVector kk = (days>cday);
    dpind[kk] = 2;
    type = 1;
  } else {
    cday = -maxdelay;
    nc = nw+2;
    type = 0;
  }
  IntegerVector missind = findmiss(outcome1);
  int nmiss = missind.size();
  NumericVector miss0 = as<NumericVector>(sample(maxdelay,nmiss,true));
  outcome1[missind] = miss0;
  // matrix of Bayesian parameter estimates
  NumericMatrix parameter (iter,nc);
  // matrix of back-calculated counts;
  NumericMatrix back (iter,nd+maxdelay);
  NumericVector par0;
  par0 = runif(nc);
  parameter(0,_) = par0;
  NumericVector oldpar;
  NumericVector newpar;
  double ratio;
  double decision;
  NumericMatrix cov;
  cov = dummy(week,weekend);
  NumericMatrix misscov (nmiss,cov.ncol());
  int mi;
  NumericVector mm;
  NumericVector par1;
  double r1;
  NumericVector mean;
  int dv;
  int nm;
  double m;
  NumericVector prob;
  IntegerVector x = seq(0,maxdelay);
  NumericVector dayseq = as<NumericVector>(x);
  NumericVector p;
  NumericVector s;
  NumericMatrix rt (iter,nd+maxdelay-size-1);
  for (int i=0;i<nmiss;++i){
    mi = missind[i];
    misscov(i,_) = cov(mi,_);
  }
  for (int i=1;i<iter;++i) { 
    oldpar = parameter(i-1,_);
    for (int k=0;k<nc;++k){
      if (k<nw+1){
        newpar = clone(oldpar);
        newpar[k] = R::rnorm(oldpar[k],sigma);
        ratio = exp(like(outcome1,cov,dpind,newpar,maxdelay) - like(outcome1,cov,dpind,oldpar,maxdelay));
        decision = (ratio>1)?1:ratio;
        parameter(i,k) = (R::runif(0,1)<decision)?newpar[k]:oldpar[k];
        oldpar[k] = parameter(i,k);
      } else {
        newpar = clone(oldpar);
        newpar[k] = exp(R::rnorm(log(oldpar[k]),sigma));
        if (newpar[k] < 100) {
          ratio = (newpar[k]/oldpar[k])*exp(like(outcome1,cov,dpind,newpar,maxdelay) - like(outcome1,cov,dpind,oldpar,maxdelay));
        } else {
          ratio = 0;
        }
        decision = (ratio>1)?1:ratio;
        parameter(i,k) = (R::runif(0,1)<decision)?newpar[k]:oldpar[k];
        oldpar[k] = parameter(i,k);
      }
    }
    // Impute the missing delays
    if (type == 1){
      par1 = oldpar[seq(0,nc-3)];
      r1 = oldpar[nc-2];
      double r2 = oldpar[nc-1];
      mm = meanvec(misscov,par1);
      mean = unique(mm);
      dv = mean.size();
      for (int k=0;k<dv;++k){
        m = mean[k];
        LogicalVector select1 = (mm==m)&(dpind==1);
        LogicalVector select2 = (mm==m)&(dpind==2);
        int nm1 = sum(select1);
        int nm2 = sum(select2);
        if (nm1>0){
          prob = Rcpp::dnbinom_mu(dayseq,r1,m);
          p = prob/sum(prob);
          s = sample(dayseq,nm1,true,p); 
          IntegerVector ind = missind[select1];
          outcome1[ind] = s;
        }
        if (nm2>0){
          prob = Rcpp::dnbinom_mu(dayseq,r2,m);
          p = prob/sum(prob);
          s = sample(dayseq,nm2,true,p); 
          IntegerVector ind = missind[select2];
          outcome1[ind] = s;
        }
      }
    } else {
    par1 = oldpar[seq(0,nc-2)];
    r1 = oldpar[nc-1];
    mm = meanvec(misscov,par1);
    mean = unique(mm);
    dv = mean.size();
    for (int k=0;k<dv;++k){
      m = mean[k];
      LogicalVector select = (mm==m);
      nm = sum(select);
      prob = Rcpp::dnbinom_mu(dayseq,r1,m);
      p = prob/sum(prob);
      s = sample(dayseq,nm,true,p); 
      IntegerVector ind = missind[select];
      outcome1[ind] = s;
    }
    }
    // Back-calculation
    NumericVector backc (nd+maxdelay);
    NumericVector backd = days-outcome1;
    for (int j=0;j<(nd+maxdelay);++j){
      backc[j] = sum(backd==(j-maxdelay+1));
    }
    // Nowcasting
    NumericVector weights = prop(outcome1,backd,maxdelay,cday);
    NumericVector back1 = backc[seq(nd,nd+maxdelay-1)];
    NumericVector check0 (back1.size());
    LogicalVector l = (back1==0);
    check0[l] = 1;
    NumericVector back2 = back1 + check0;  
    NumericVector trunc = mapply(back2,weights,rnb);
    NumericVector now = back1+trunc;
    NumericVector now1 = now - check0;
    LogicalVector check = (now1<0);
    now1[check] = 0;
    backc[seq(nd,nd+maxdelay-1)] = now1;
    back(i,_) = backc;
    rt(i,_) = getr(backc,si,size);
  }
  List output = List::create(Named("Back")=back,Named("R")=rt);
  return output;
}












