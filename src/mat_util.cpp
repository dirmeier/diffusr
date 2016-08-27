/**
 * @author Simon Dirmeier
 * @email simon.dirmeier@bsse.ethz.ch
 */

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

//' Column normalize a matrix, so that it is stochastic.
//'
//' @noRd
//' @param W  the adjacency matrix to be normalized
//' @return  returns the normalized matrix
// [[Rcpp::export]]
SEXP do_stoch_col_norm(Eigen::MatrixXd W) {
  Eigen::MatrixXd res(W.rows(), W.cols());
  Eigen::VectorXd colsums = W.colwise().sum();
  const double empt_col_val = 1.0 / W.rows();
  for (unsigned int i = 0; i < W.cols(); ++i )
  {
    if ((W.col(i)).all() == 0.0) res.col(i).fill(empt_col_val);
    else res.col(i) = W.col(i) / colsums(i);
  }
  Rcpp::S4 Wout(Rcpp::wrap(res));
  return Wout;
}

