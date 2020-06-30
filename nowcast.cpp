#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppArmadillo)]]



// back-calculation function
// [[Rcpp::export]]
NumericVector backcal(double count, Date d, int wind, int maxdelay, NumericVector par){
  int npar = par.size();
  double regpar = par[0];
  NumericVector wpar = par[seq(1,npar-3)];
  double wdpar = par[npar-2];
  double r = par[npar-1];
  int day = d.getWeekday();
  int weekend; 
  weekend = ((day==1)|(day==7))?1:0;
  double mean = exp(regpar+wpar[wind-1]+weekend*wdpar);
  IntegerVector dd = rev(seq(0,maxdelay));
  NumericVector prob = Rcpp::dnbinom_mu(dd,r,mean);
  NumericVector prob1 = prob/sum(prob);
  NumericVector onsetn = round(count*prob1,0);
  return onsetn;
}


// adjday function
// [[Rcpp::export]]
NumericVector adjcurve(NumericVector counts, DateVector dates, IntegerVector wind, int maxdelay, NumericVector par){
  int nc = counts.size();
  int nd = dates.size();
  int nw = par.size()-3;
  if (nc!=nd){
    stop("The number of report cast counts does not match the number of report days");
  }
  if (nc!=wind.size()){
    stop("The number of report cast counts does not match the number of week indicators");
  }
  if ((max(wind) > nw)|(min(wind) < 1)){
    stop("Illegal week indicators; Has to be integers between 1 and %i",nw);
  }
  if (nd < maxdelay) {
    stop("Maxdelay has to be smaller than the number of days");
  }
  int nr = nd+maxdelay;
  NumericVector result(nr*nd);
  NumericVector t;
  for (int i=0;i<nd;++i){
    t = backcal(counts[i],dates[i],wind[i],maxdelay,par);
    result[seq((nr+1)*i,(nr+1)*i+maxdelay)] = t;
  }
  result.attr("dim") = Dimension(nr,nd);
  NumericMatrix result1 = as<NumericMatrix>(result);
  NumericVector out = rowSums(result1);
  return out;
}


// create a lambda function to compute mean of poisson dist.
// function used in getr and updater
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


// curve is the adjusted epidemic curve
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



// a helper for missp
// get indicator of whether a day is on weekend
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



// function used by mapply
double nb(double x, double r, double m){
  double y = R::dnbinom_mu(x,r,m,false);
  return y;
}



// calculate the missing weights for nowcasting
// [[Rcpp::export]]
NumericVector missp(DateVector dates, IntegerVector wind, int maxdelay, NumericVector par){
  int nd = dates.size();
  int npar = par.size();
  int nw = npar - 3;
  if (nd < maxdelay){
    stop("Maxdelay should be smaller than the number of days available");
  }
  if (nd < 2*maxdelay) {
    warning("It is recommended that maxdelay is smaller than the number of original reporting dates");
  }
  if ((nd-maxdelay)!= wind.size()){
    stop("The number of dates does not match the number of week indicators");
  }
  if ((max(wind) > nw)|(min(wind) < 1)){
    stop("Illegal week indicators; Has to be integers between 1 and %i",nw);
  }
  IntegerVector wind1 (nd+2*maxdelay);
  wind1[seq(maxdelay,maxdelay+nd-1)] = wind-1;
  wind1[seq(nd,nd+maxdelay-1)] = rep(nw-1,maxdelay); 
  double regpar = par[0];
  NumericVector wpar = par[seq(1,npar-3)];
  double wdpar = par[npar-2];
  double r = par[npar-1];
  IntegerVector ind = seq(nd-maxdelay,nd-1);
  NumericVector result (maxdelay);
  DateVector d1 = dates[seq(nd-maxdelay,nd-1)];
  NumericVector wd;
  NumericVector mean;
  NumericVector numerator;
  double denominator;
  NumericVector k = rep(r,maxdelay+1);
  IntegerVector g = rev(seq(0,maxdelay));
  NumericVector p;
  for (int i=0;i<maxdelay;++i){
    int ind1 = ind[i];
    IntegerVector s = rev(seq(ind1,ind1+maxdelay));
    IntegerVector week = wind1[s];
    wd = weekend(d1[i],maxdelay);
    NumericVector w1 = wd*wdpar+regpar;
    NumericVector w2 = wpar[week];
    mean = exp(w1+w2);
    numerator = mapply(g,k,mean,nb);
    denominator = sum(numerator);
    p = numerator/denominator;
    result[i] = sum(p[seq(0,i)]);
  }
  return result;
}




// Dynamic nowcasting method
// [[Rcpp::export]]
NumericVector dynamic(NumericVector counts, NumericVector backn, NumericVector lowern, NumericVector missp,
                      NumericVector si, int maxdelay, double r0, int ws){
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
  for (int i=0;i<maxdelay;++i){
    back = backn1[seq(i,i+k-1)];
    lambda[i] = r0*sum(rev(back)*si)*missp[i];
    value[0] = ((lowern[i]+lambda[i]-2*sqrt(sum(lambda)))>0)?(lowern[i]+lambda[i]-2*sqrt(sum(lambda))):0;
    value[1] = lowern[i]+lambda[i];
    value[2] = lowern[i]+lambda[i]+2*sqrt(sum(lambda));
    value1 = round(value,0);
    c[seq(3*i,3*i+2)] = round(value,0);
    r[seq(3*i,3*i+2)] = updater(counts1,si,ws,n-maxdelay+i,value1);
    counts1[n-maxdelay+i] = value1[1];
    backn1[i+k] = value1[1];
    r0 = r[3*i+1];
  }
  result[seq(0,3*maxdelay-1)] = c;
  result[seq(3*maxdelay,6*maxdelay-1)] = r;
  return result;
}



// Fixed nowcasting method (with fixed R0)
// [[Rcpp::export]]
NumericVector fixed(NumericVector counts, NumericVector backn, NumericVector lowern, NumericVector missp,
                      NumericVector si, int maxdelay, double r0, int ws){
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
  for (int i=0;i<maxdelay;++i){
    back = backn1[seq(i,i+k-1)];
    lambda[i] = r0*sum(rev(back)*si)*missp[i];
    value[0] = ((lowern[i]+lambda[i]-2*sqrt(sum(lambda)))>0)?(lowern[i]+lambda[i]-2*sqrt(sum(lambda))):0;
    value[1] = lowern[i]+lambda[i];
    value[2] = lowern[i]+lambda[i]+2*sqrt(sum(lambda));
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


// Specific nowcasting method (with specific values of R0)
// [[Rcpp::export]]
NumericVector specific(NumericVector counts, NumericVector backn, NumericVector lowern, NumericVector missp,
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
  for (int i=0;i<maxdelay;++i){
    back = backn1[seq(i,i+k-1)];
    lambda[i] = r0[i]*sum(rev(back)*si)*missp[i];
    value[0] = ((lowern[i]+lambda[i]-2*sqrt(sum(lambda)))>0)?(lowern[i]+lambda[i]-2*sqrt(sum(lambda))):0;
    value[1] = lowern[i]+lambda[i];
    value[2] = lowern[i]+lambda[i]+2*sqrt(sum(lambda));
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
NumericVector nowcast(NumericVector curve, NumericVector missp, NumericVector si, int maxdelay, int size, 
                      String method = "dynamic", bool useR = false, NumericVector r0 = 0){
  int n = curve.size();
  int k = si.size();
  int m = missp.size();
  if (m!=maxdelay){
    stop("The length of missp does not match the number of nowcasted days");
  }
  if (n<(k+m)){
    stop("Does not have enough observations. You should at least have %i complete observations", k+m);
  }
  CharacterVector methods = {"dynamic","fixed","specific"}; 
  LogicalVector s (3);
  for (int i=0;i<3;++i){
    s[i] = (methods[i] == method);
  }
  bool s1 = is_false(any(s));
  if(s1){
    stop("Does not have a proper method; The method argument needs to be dynamic, fixed or specific");
  }
  NumericVector lowern = curve[seq(n-maxdelay,n-1)];
  NumericVector backn = curve[seq(n-maxdelay-k,n-maxdelay-1)];
  NumericVector reprod = getr(curve,si,size);
  NumericVector output (6*maxdelay);
  double r1;
  if (method=="dynamic"){
    r1 = reprod[n-maxdelay-size-2];
    output = dynamic(curve,backn,lowern,missp,si,maxdelay,r1,size);
  }
  if (method=="fixed"){
    if (useR) {
      r1 = r0[0];
    } else {
      r1 = reprod[n-maxdelay-size-2];
    }
    output = fixed(curve,backn,lowern,missp,si,maxdelay,r1,size);
  }
  if (method=="specific"){
    if (!useR){
      stop("The useR argument has to be true for specific method");
    }
    if (r0.size()!=maxdelay){
      stop("The size of your r0 input must be equal to i%.", maxdelay);
    }
    if (is_true(any(r0<0.1))){
      stop("The elements in your r0 input should be at least 0.1.");
    }
    output = specific(curve,backn,lowern,missp,si,maxdelay,r0,size);
  }
  
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
// Special Note: We only need ld (the last reporting day)
// The output should be a list of two dataframes, the first one is for counts and the second one is for R
// [[Rcpp::export]]
List postprocess(NumericVector reportn, Date ld, IntegerVector wind, int maxdelay, 
                 NumericMatrix par, int k, double shape, double scale, int size, String method = "dynamic",
                 bool useR = false, NumericVector r0 = 0){
  int nr = par.nrow();
  int nd = reportn.size();
  DateVector dic = dategen(ld,nd); // all the reporting dates
  DateVector dc = dategen(ld,nd+maxdelay); // all possible dates
  NumericVector si = sip(k,shape,scale); // serial interval
  // cc is the matrix for complete case counts
  NumericMatrix cc (nr,nd);
  // rc is the matrix for reproductive number estimate based on complete case counts
  NumericMatrix rc (nr,nd-size-1);
  // cic is the matrix for incomplete/nowcasted case counts
  NumericMatrix cic (nr,3*maxdelay);
  // ric is the matrix for reproductive number estimate based on nowcasted case counts
  NumericMatrix ric (nr, 3*maxdelay);
  NumericVector curve; // adjusted curve given by the adjcurve function
  NumericVector para; // parameters: each row of par
  NumericVector rt; // reproductive number estimates based on adjusted curve
  NumericVector miss; // the missing weight for nowcasting
  NumericVector now; // vector of nowcasting
  for (int i=0;i<nr;++i){
    para = par(i,_);
    curve = adjcurve(reportn,dic,wind,maxdelay,para);
    cc(i,_) = curve[seq(0,nd-1)];
    rt = getr(curve,si,size);
    rc(i,_) = rt[seq(0,nd-size-2)];
    miss = missp(dc,wind,maxdelay,para);
    now = nowcast(curve,miss,si,maxdelay,size,method,useR,r0);
    cic(i,_) = now[seq(0,3*maxdelay-1)];
    ric(i,_) = now[seq(3*maxdelay,6*maxdelay-1)];
  }
  NumericVector nowrl (maxdelay);
  NumericVector nowru (maxdelay);
  NumericVector nowrm (maxdelay);
  NumericVector nowcl (maxdelay);
  NumericVector nowcu (maxdelay);
  NumericVector nowcm (maxdelay);
  NumericVector p1 (1);
  NumericVector p2 (1);
  NumericVector p3 (1);
  p1[0] = 0.025;
  p2[0] = 0.5;
  p3[0] = 0.975;
  for (int i=0;i<maxdelay;++i){
    nowcl[i] = as<double>(qf(cic(_,3*i),p1));
    nowcm[i] = as<double>(qf(cic(_,3*i+1),p2));
    nowcu[i] = as<double>(qf(cic(_,3*i+2),p3));
    nowrl[i] = as<double>(qf(ric(_,3*i),p1));
    nowrm[i] = as<double>(qf(ric(_,3*i+1),p2));
    nowru[i] = as<double>(qf(ric(_,3*i+2),p3));
  }
  NumericMatrix backc (nd,3);
  NumericVector p = NumericVector::create(0.025,0.5,0.975);
  for (int i=0;i<nd;++i){
    backc(i,_) = qf(cc(_,i),p);
  }
  NumericMatrix backr (nd-size-1,3);
  int n1 = nd-size-1;
  for (int i=0;i<n1;++i){
    backr(i,_) = qf(rc(_,i),p);
  }
  NumericVector lowr (nd+maxdelay-size-1);
  NumericVector medr (nd+maxdelay-size-1);
  NumericVector upr (nd+maxdelay-size-1);
  NumericVector lowc (nd+maxdelay);
  NumericVector medc (nd+maxdelay);
  NumericVector upc (nd+maxdelay);
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
  DataFrame caseno = DataFrame::create(Named("lower")=lowc,Named("median")=medc,Named("upper")=upc,Named("date")=dc);
  DataFrame rest = DataFrame::create(Named("lower")=lowr,Named("median")=medr,Named("upper")=upr,Named("date")=dr);
  List result = List::create(Named("Count")=caseno,Named("R")=rest);
  return result;
}












