/**
 * @author Simon Dirmeier
 * @email simon.dirmeier@bsse.ethz.ch
 */

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <vector>

//' Do a Markon random walk (with restart) on an column-normalised adjacency matrix.
//'
//' @noRd
//' @param p0  the staring distribution
//' @param W  the column normalized adjacency matrix
//' @param r  restart probability
//' @return  returns the stationary distribution p_inf
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export(name=".mrwr_cpp")]]
Eigen::VectorXd mrwr_(const Eigen::VectorXd& p0,
                      const Eigen::MatrixXd& W,
                      const double r)
{
  Eigen::VectorXd pt = p0;
  Eigen::VectorXd pold;
  const double thresh = .00001;
  do
  {
    pold = pt;
    pt = (1  - r) * W * pold + r * p0;
  }
  while ((pt - pold).norm() > thresh);
  return pt;
}




