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



// function used by mapply
double nb(double x, double r, double m){
  double y = R::dnbinom_mu(x,r,m,true);
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



// [[Rcpp::export]]
List mcmc(NumericVector outcome, NumericVector days, IntegerVector week, IntegerVector weekend, 
                   int iter, double sigma, int maxdelay){
  int nw = max(week);
  int nc = nw+2;
  int nd = max(days);
  IntegerVector missind = findmiss(outcome);
  int nmiss = missind.size();
  NumericVector miss0 = as<NumericVector>(sample(maxdelay,nmiss,true));
  outcome[missind] = miss0;
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
  NumericVector dayseq (maxdelay+1);
  NumericVector p;
  NumericVector s;
  for (int i=0;i<(maxdelay+1);++i){
    dayseq[i] = i; 
  }
  for (int i=0;i<nmiss;++i){
    mi = missind[i];
    misscov(i,_) = cov(mi,_);
  }
  for (int i=1;i<iter;++i) { 
    oldpar = parameter(i-1,_);
    for (int k=0;k<nc;++k){
      if (k<nc-1){
        newpar = clone(oldpar);
        newpar[k] = R::rnorm(oldpar[k],sigma);
        ratio = exp(like(outcome,cov,newpar) - like(outcome,cov,oldpar));
        decision = (ratio>1)?1:ratio;
        parameter(i,k) = (R::runif(0,1)<decision)?newpar[k]:oldpar[k];
        oldpar[k] = parameter(i,k);
      } else {
        newpar = clone(oldpar);
        newpar[k] = exp(R::rnorm(log(oldpar[k]),sigma));
        if (newpar[k] < 100) {
          ratio = (newpar[k]/oldpar[k])*exp(like(outcome,cov,newpar) - like(outcome,cov,oldpar));
        } else {
          ratio = 0;
        }
        decision = (ratio>1)?1:ratio;
        parameter(i,k) = (R::runif(0,1)<decision)?newpar[k]:oldpar[k];
        oldpar[k] = parameter(i,k);
      }
    }
    par1 = oldpar[seq(0,nc-2)];
    r1 = oldpar[nc-1];
    // Impute the missing delays
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
      outcome[ind] = s;
    }
    // Back-calculation
    NumericVector backc (nd+maxdelay);
    NumericVector backd = days-outcome;
    for (int j=0;j<(nd+maxdelay);++j){
      backc[j] = sum(backd==(j-maxdelay+1));
    }
    back(i,_) = backc;
  }
  List output = List::create(Named("Parameter")=parameter,Named("Back")=back);
  return output;
}



// create a lambda function to compute mean of poisson dist.
// function used in getr and updater
// curve is the estimated epidemic curve (i.e., back-calculated counts & nowcasted counts)
// [[Rcpp::export]]
NumericVector lambda(NumericVector curve, NumericVector si){
  int k = si.size();
  int n = curve.size();
  if (sum(si)!=1){
    stop("The sum of serial interval must equal to 1 exactly");
  }
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


// the updater function
// [[Rcpp::export]]
NumericVector updater(NumericVector curve, NumericVector si, int size, int loc, NumericVector newvalue){
  int n1 = newvalue.size();
  NumericVector result (n1);
  NumericVector incid = curve[seq(loc-size,loc)];
  NumericVector dem = lambda(curve,si);
  NumericVector dem1 = dem[seq(loc-size-1,loc-1)];
  double shape;
  double scale;
  for (int i=0;i<n1;++i){
    incid[size] = newvalue[i];
    shape = sum(incid)+1;
    scale = 1/(sum(dem1)+0.2);
    result[i] = shape*scale ; 
  }
  return result;
}



// a helper for the weight function
// get indicator of whether a day is on weekend 
// for a series of date from d+maxdelay to d
NumericVector weekend(Date d, int maxdelay){
  IntegerVector s = rev(seq(0,maxdelay));
  int wd1 = d.getWeekday();
  if (wd1==7) {
    wd1 = 0;
  }
  IntegerVector wd (maxdelay+1);
  wd[maxdelay] = wd1;
  for(int i=0;i<maxdelay;++i){
    int ind = s[i];
    int x = (wd1+(ind%7)+7)%7;
    wd[i] = x;
  }
  LogicalVector l = (wd<2);
  NumericVector result = ifelse(l,1.0,0.0);
  return result;
}


// another helper function for the weight function
// function used by mapply
double nb1(double x, double r, double m){
  double y = R::dnbinom_mu(x,r,m,false);
  return y;
}



// calculate the missing weights for nowcasting
// wind is of size n
// dates is of size n+maxdelay
// count is of size n+maxdelay (with both observed reportn and future reportn included; Need to make sure it is for consecutive days)
// [[Rcpp::export]]
NumericVector weight(DateVector dates, NumericVector count, IntegerVector wind, int maxdelay, NumericVector par){
  int nd = dates.size(); // total number of back-calculated dates
  int n = wind.size();
  int npar = par.size();
  int nw = npar - 2;
  if (n < maxdelay) {
    stop("The maxdelay should be no larger than %i",n);
  }
  if ((nd-maxdelay)!= n){
    stop("The number of dates does not match the number of week indicators");
  }
  if ((max(wind) != nw)|(min(wind) != 1)){
    stop("Illegal week indicator or a mismatch between the length of parameters and the number of weeks provided");
  }
  if (count.size() != (n+maxdelay)){
    stop("The count has to be of size %i", n+maxdelay);
  }
  NumericVector counts (nd+maxdelay);
  counts[seq(maxdelay,nd+maxdelay-1)] = count;
  IntegerVector wind1 (nd+maxdelay);
  wind1[seq(maxdelay,nd-1)] = wind-1; // dates earlier than the earliest reporting date will receive 0
  wind1[seq(nd,nd+maxdelay-1)] = rep(nw-1,maxdelay); // dates later than the last reporting date will receive nw-1
  NumericVector wpar = par[seq(0,npar-3)];
  double wdpar = par[npar-2];
  double r = par[npar-1];
  IntegerVector ind = seq(nd-maxdelay,nd-1);
  NumericVector result (maxdelay);
  DateVector d1 = dates[seq(nd-maxdelay,nd-1)]; // dates need nowcasting
  NumericVector wd;
  NumericVector mean;
  NumericVector numerator;
  double denominator;
  NumericVector k = rep(r,maxdelay+1);
  IntegerVector g = rev(seq(0,maxdelay));
  NumericVector p;
  NumericVector count1;
  NumericVector prior;
  NumericVector like;
  for (int i=0;i<maxdelay;++i){
    int ind1 = ind[i];
    IntegerVector s = rev(seq(ind1,ind1+maxdelay));
    count1 = counts[s];
    prior = count1/sum(count1);
    IntegerVector week = wind1[s];
    wd = weekend(d1[i],maxdelay);
    NumericVector w1 = wd*wdpar;
    NumericVector w2 = wpar[week];
    mean = exp(w1+w2);
    like = mapply(g,k,mean,nb1);
    numerator = like*prior;
    denominator = sum(numerator);
    p = numerator/denominator;
    result[i] = sum(p[seq(0,i)]);
  }
  return (1-result);
}



// The nonparametric method 
// the weights are given by the weight function
// curve is the back-calculated counts
// the size of curve is n+maxdelay (n is the size of original reports)
// [[Rcpp::export]]
NumericVector npest(NumericVector curve, NumericVector weights, NumericVector si, int maxdelay, int ws){
  NumericVector result (2*maxdelay);
  NumericVector curve1 = clone(curve);
  int n = curve.size();
  NumericVector oldcount = curve1[seq(n-maxdelay,n-1)];
  NumericVector newcount = round(oldcount/weights,0);
  curve1[seq(n-maxdelay,n-1)] = newcount;
  NumericVector r = getr(curve1,si,ws);
  int nr = r.size();
  NumericVector r1 = r[seq(nr-maxdelay,nr-1)];
  result[seq(0,maxdelay-1)] = newcount;
  result[seq(maxdelay,2*maxdelay-1)] = r1;
  return result;
}



// The nonparametric nowcasting function
// curve and dates are the back-calculated counts and their corresponding dates
// the size of curve and dates is n+maxdelay (n is the size of reportn)
// n0 is the future daily counts
// [[Rcpp::export]]
NumericVector npnow(NumericVector curve, DateVector dates, NumericVector reportn, IntegerVector wind, int maxdelay, 
                    NumericVector par, NumericVector si, int ws, NumericVector n0){
  int n = curve.size();
  int k = si.size();
  if (n<(k+maxdelay+1)){
    stop("Does not have enough observations. You should at least have %i complete observations", k+1);
  }
  if (k<ws) {
    stop("The length of serial interval should be larger than the sliding window size");
  }
  if (n0.size() != maxdelay){
    stop("The size of n0 needs to be %i",maxdelay);
  }
  if (is_true(any(n0<=0))){
    stop("The elements of n0 needs to be positive numbers");
  }
  NumericVector output (2*maxdelay);
  NumericVector extra_report (n);
  extra_report[seq(0,n-maxdelay-1)] = reportn;
  extra_report[seq(n-maxdelay,n-1)] = n0;
  NumericVector weights = weight(dates,extra_report,wind,maxdelay,par);
  output = npest(curve,weights,si,maxdelay,ws);
  return output;
}



// Specific nowcasting method (with specific values of R0)
// [[Rcpp::export]]
NumericVector specific(NumericVector counts, NumericVector backn, NumericVector lowern,
                    NumericVector si, int maxdelay, NumericVector r0, int ws){
  int n = counts.size();
  NumericVector counts1 = clone(counts);
  NumericVector lambda (maxdelay);
  NumericVector c (3*maxdelay);
  NumericVector r (3*maxdelay);
  NumericVector result (6*maxdelay);
  NumericVector value (3);
  NumericVector value1 (3);
  int k = backn.size();
  NumericVector backn1 (k+maxdelay);
  backn1[seq(0,k-1)] = backn;
  NumericVector back;
  double m;
  for (int i=0;i<maxdelay;++i){
    back = backn1[seq(i,i+k-1)];
    m = r0[i]*sum(rev(back)*si);
    lambda[i] = (m>lowern[i])?m:lowern[i];
    value[0] = ((lambda[i]-2*sqrt(sum(lambda)))>0)?(lambda[i]-2*sqrt(sum(lambda))):0;
    value[1] = lambda[i];
    value[2] = lambda[i]+2*sqrt(sum(lambda));
    value1 = round(value,0);
    c[seq(3*i,3*i+2)] = round(value,0);
    r[seq(3*i,3*i+2)] = updater(counts1,si,ws,n-maxdelay+i,value1);
    counts1[n-maxdelay+i] = value1[1];
    backn1[i+k] = value1[1];
  }
  result[seq(0,3*maxdelay-1)] = c;
  result[seq(3*maxdelay,6*maxdelay-1)] = r;
  return result;
}



// Finally, the nowcasting function, with all pieces gathered
// [[Rcpp::export]]
NumericVector pnow(NumericVector curve, NumericVector si, int maxdelay, int size, NumericVector r0 = 0){
  int n = curve.size();
  int k = si.size();
  if (n<(k+maxdelay+1)){
    stop("Does not have enough observations. You should at least have %i complete observations", k+1);
  }
  if (k<size) {
    stop("The length of serial interval should be larger than the sliding window size");
  }
  NumericVector lowern = curve[seq(n-maxdelay,n-1)];
  NumericVector backn = curve[seq(n-maxdelay-k,n-maxdelay-1)];
  NumericVector reprod = getr(curve,si,size);
  NumericVector output (6*maxdelay);
  if (r0.size()!=maxdelay){
    stop("The size of your r0 input must be equal to i%.", maxdelay);
  }
  if (is_true(any(r0<0.1))){
    stop("The elements in your r0 input should be at least 0.1.");
  }
  output = specific(curve,backn,lowern,si,maxdelay,r0,size);
  return output;
}



// Last, we need a function to calculate the serial interval based on gamma distribution
// [[Rcpp::export]]
NumericVector sip(int ndays, double shape, double scale){
  NumericVector probs (ndays);
  for (int i=0;i<ndays;++i){
    probs[i] = R::pgamma(i+1,shape,scale,true,false)-R::pgamma(i,shape,scale,true,false);
  }
  NumericVector result = probs/sum(probs);
  return result;
}



// Helper function for generating complete dates based on user input reporting dates
// [[Rcpp::export]]
DateVector dategen(Date d, int n){
  DateVector result (n);
  for (int i=0;i<n;++i){
    result[i] = d +(-(n-i-1));
  }
  return result;
}



// import the R quantile function
// [[Rcpp::export]]
NumericVector qf(NumericVector x, NumericVector probs) {
  Environment stats("package:stats");
  Function quantile = stats["quantile"];
  int npr = probs.size();
  NumericVector ans(npr);
  for(int i=0; i<npr; i++){
    ans[i] = as<double>(quantile(x, probs[i]));
  }
  return ans;
}



// Gather everything to produce desired output
// Special Note: We only need ld (the last reporting day), but need to make sure reportn is defined for consecutive days
// Make sure the rows of par and the row of backn are the same
// The output should be a list of two dataframes, the first one is for counts and the second one is for R
// [[Rcpp::export]]
List postprocess(NumericVector reportn, Date ld, IntegerVector wind, int maxdelay, NumericMatrix backn,
                 NumericMatrix par, int k, double shape, double scale, int size, bool np = true,
                 NumericVector r0 = 0, NumericVector n0 = 0){
  int nr = par.nrow();
  int nb = backn.nrow();
  int nd = reportn.size();
  if (nr!=nb) {
    stop("The number of rows of the parameter matrix does not equal to the number of rows of the back-calculated count matrix");
  }
  DateVector dic = dategen(ld,nd); // all the reporting dates
  DateVector dc = dategen(ld,nd+maxdelay); // all possible dates
  NumericVector si = sip(k,shape,scale); // serial interval
  NumericVector nowrl (maxdelay);
  NumericVector nowru (maxdelay);
  NumericVector nowrm (maxdelay);
  NumericVector nowcl (maxdelay);
  NumericVector nowcu (maxdelay);
  NumericVector nowcm (maxdelay);
  NumericVector lowr (nd+maxdelay-size-1);
  NumericVector medr (nd+maxdelay-size-1);
  NumericVector upr (nd+maxdelay-size-1);
  NumericVector lowc (nd+maxdelay);
  NumericVector medc (nd+maxdelay);
  NumericVector upc (nd+maxdelay);
  NumericMatrix backc (nd,3);
  NumericMatrix backr (nd-size-1,3);
  int n1 = nd-size-1; // number of reproductive number estimates
  NumericVector p = NumericVector::create(0.025,0.5,0.975);
  // cc is the matrix for complete case counts
  NumericMatrix cc (nr,nd);
  // rc is the matrix for reproductive number estimate based on complete case counts
  NumericMatrix rc (nr,nd-size-1);
  NumericVector curve; // adjusted curve given by the backn
  NumericVector rt; // reproductive number estimates based on adjusted curve
  NumericVector now; // vector of nowcasting
  NumericVector p1 (1);
  NumericVector p2 (1);
  NumericVector p3 (1);
  p1[0] = 0.025;
  p2[0] = 0.5;
  p3[0] = 0.975;
  if (np) {
    NumericVector para; // parameters: each row of par
    // cic is the matrix for incomplete/nowcasted case counts (nonparametric)
    NumericMatrix cic (nr,maxdelay);
    // ric is the matrix for reproductive number estimate based on nowcasted case counts (nonparametric)
    NumericMatrix ric (nr,maxdelay);
    NumericMatrix nowc (maxdelay,3);
    NumericMatrix nowr (maxdelay,3);
    for (int i=0;i<nr;++i){
      para = par(i,_);
      curve = backn(i,_);
      cc(i,_) = curve[seq(0,nd-1)];
      rt = getr(curve,si,size);
      rc(i,_) = rt[seq(0,nd-size-2)];
      now = npnow(curve,dc,reportn,wind,maxdelay,para,si,size,n0);
      cic(i,_) = now[seq(0,maxdelay-1)];
      ric(i,_) = now[seq(maxdelay,2*maxdelay-1)];
    }
    for (int i=0;i<maxdelay;++i){
      nowc(i,_) = qf(cic(_,i),p);
      nowr(i,_) = qf(ric(_,i),p);
    }
    nowcl = nowc(_,0);
    nowcm = nowc(_,1);
    nowcu = nowc(_,2);
    nowrl = nowr(_,0);
    nowrm = nowr(_,1);
    nowru = nowr(_,2);
  } else {
    // cic is the matrix for incomplete/nowcasted case counts (parametric)
    NumericMatrix cic (nr,3*maxdelay);
    // ric is the matrix for reproductive number estimate based on nowcasted case counts (parametric)
    NumericMatrix ric (nr, 3*maxdelay);
    for (int i=0;i<nr;++i){
      curve = backn(i,_);
      cc(i,_) = curve[seq(0,nd-1)];
      rt = getr(curve,si,size);
      rc(i,_) = rt[seq(0,nd-size-2)];
      now = pnow(curve,si,maxdelay,size,r0);
      cic(i,_) = now[seq(0,3*maxdelay-1)];
      ric(i,_) = now[seq(3*maxdelay,6*maxdelay-1)];
    }
    for (int i=0;i<maxdelay;++i){
      nowcl[i] = as<double>(qf(cic(_,3*i),p1));
      nowcm[i] = as<double>(qf(cic(_,3*i+1),p2));
      nowcu[i] = as<double>(qf(cic(_,3*i+2),p3));
      nowrl[i] = as<double>(qf(ric(_,3*i),p1));
      nowrm[i] = as<double>(qf(ric(_,3*i+1),p2));
      nowru[i] = as<double>(qf(ric(_,3*i+2),p3));
    }
  }
  for (int i=0;i<nd;++i){
    backc(i,_) = qf(cc(_,i),p);
  }
  for (int i=0;i<n1;++i){
    backr(i,_) = qf(rc(_,i),p);
  }
  lowr[seq(0,n1-1)] = backr(_,0);
  lowr[seq(n1,n1+maxdelay-1)] = nowrl;
  medr[seq(0,n1-1)] = backr(_,1);
  medr[seq(n1,n1+maxdelay-1)] = nowrm;
  upr[seq(0,n1-1)] = backr(_,2);
  upr[seq(n1,n1+maxdelay-1)] = nowru;
  lowc[seq(0,nd-1)] = backc(_,0);
  lowc[seq(nd,nd+maxdelay-1)] = nowcl;
  medc[seq(0,nd-1)] = backc(_,1);
  medc[seq(nd,nd+maxdelay-1)] = nowcm;
  upc[seq(0,nd-1)] = backc(_,2);
  upc[seq(nd,nd+maxdelay-1)] = nowcu;
  DateVector dr = dc[seq(1+size,nd+maxdelay-1)];
  DataFrame caseno = DataFrame::create(Named("lower")=round(lowc,0),Named("median")=round(medc,0),Named("upper")=round(upc,0),Named("date")=dc);
  DataFrame rest = DataFrame::create(Named("lower")=lowr,Named("median")=medr,Named("upper")=upr,Named("date")=dr);
  List result = List::create(Named("Count")=caseno,Named("R")=rest);
  return result;
}












