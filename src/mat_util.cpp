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
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export(name=".stoch_col_norm_cpp")]]
Eigen::MatrixXd stoch_col_norm_(const Eigen::MatrixXd& W)
{
  Eigen::MatrixXd res(W.rows(), W.cols());
  Eigen::VectorXd colsums = W.colwise().sum();
  const double empt_col_val = 1.0 / W.rows();
  const double zero_col = 0.00001;
  for (unsigned int i = 0; i < W.cols(); ++i )
  {
    if ((W.col(i)).sum() <= zero_col) res.col(i).fill(empt_col_val);
    else res.col(i) = W.col(i) / colsums(i);
  }
  return res;
}

