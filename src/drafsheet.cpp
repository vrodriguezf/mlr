// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

//' @export
// [[Rcpp::export]]
List MstepMultinomial(const arma::ivec &x, const arma::fmat &wt) {
  int J = wt.n_cols;
  IntegerVector x_rcpp = IntegerVector(x.begin(), x.end());
  int sizeDiscreteSpace = unique(na_omit(x_rcpp)).length();
  //Rcout << na_omit(x_rcpp) << "\n";

  arma::fmat ans(sizeDiscreteSpace, J);
  for (int i = 0; i < sizeDiscreteSpace; i++)
    for (int j = 0; j < J; j++) {
      arma::uvec ids = find(x == (i+1));
      arma::fvec wt_col = wt.col(j);
      ans(i, j) = arma::accu(wt_col.elem(ids))/arma::accu(wt_col);// FIXME NAs in accu(wt_col)?? Is it possible?
    }

  // arma::uvec asd = find(x == 1);
  // arma::uvec v = wt.col(0);
  // Rcout << arma::accu(v.elem(asd)) << "\n";
  // Rcout << arma::accu(wt.col(0)) << "\n";
  // Rcout << arma::accu(wt(0,0)) << "\n";

  return List::create(Named("pmf") = ans);
}

// mstep.multinom = function(x, wt) {
//   J = ncol(wt) # Number of states
//   size.discrete.space = length(unique(x[!is.na(x)]))
//   ans <- matrix(nrow = size.discrete.space, ncol = J)
//   for (i in 1:size.discrete.space)
//     for (j in 1:J)
//       ans[i, j] <- sum(wt[which(x[!is.na(x)] == i), j])/sum(wt[!is.na(x), j])
//       ans[i, j] <- sum(wt[which(x == i), j])/sum(wt[, j], na.rm = TRUE) NUEVO
//       list(pmf = ans)
// }


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
x = c(1, NA, 3, 2, 1, 3, 2, NA)
wt = matrix(rnorm(3*length(x)), ncol = 3)
wt = abs(wt)
ans = MstepMultinomial(x, wt)
ans
# x = rnorm(1e+7)
# foo1 = function(x, a) return(x[which(x > a)])
# foo2 = function(x, a) return(x[c_which(x, function(x, bla) { as.integer(x > bla) }, a)])
# all.equal(foo1(x, 0.1), foo2(x, 0.1))
# rbenchmark::benchmark(foo1(x, 0.1), foo2(x, 0.1), order = 'relative')[, 1:4]
*/
