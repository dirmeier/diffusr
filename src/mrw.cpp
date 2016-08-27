/**
 * @author Simon Dirmeier
 * @email simon.dirmeier@bsse.ethz.ch
 */

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

//' Do a Markon random walk (with restart) on an column-normalized adjacency matrix.
//'
//' @noRd
//' @param p0  the staring distribution
//' @param W  the column normalized adjacency matrix
//' @param r  restart probability
//' @return  returns the stationary distribution p_inf
// [[Rcpp::export]]
Eigen::VectorXd do_mrwr(Eigen::VectorXd p0, Eigen::VectorXd W, double r) {
  Eigen::VectorXd pt = p0;
  Eigen::VectorXd pold;
  do
  {
    pold = pt;
    pt = (1  - r) * W * pold + r * p0;
  }
  while ((pt - pold).sum() >  .000001);
  return pt;
}




