/**
 * @author Simon Dirmeier
 * @email simon.dirmeier@bsse.ethz.ch
 */

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

Eigen::VectorXd mrwr_(const Eigen::VectorXd& p0,
                      const Eigen::SparseMatrix<double>& W,
                      double r){
  Eigen::VectorXd pt = Eigen::VectorXd::Zero(W.rows());
  Eigen::VectorXd pold = Eigen::VectorXd::Zero(W.rows());
  do
  {
    pold = pt;
    pt = (1  - r) * W * pold + r * p0;
  }
  while ((pt-pold).sum() >  .00001);
  return pt;
}

Eigen::VectorXd mrwr_(const Eigen::VectorXd& p0,
                      const Eigen::MatrixXd& W,
                      double r){
  Eigen::VectorXd pt = Eigen::VectorXd::Zero(W.rows());
  Eigen::VectorXd pold = Eigen::VectorXd::Zero(W.rows());
  do
  {
    pold = pt;
    pt = (1  - r) * W * pold + r * p0;
  }
  while ((pt-pold).sum() >  .00001);
  return pt;
}

// [[Rcpp::export]]
Eigen::VectorXd mrwr(Eigen::VectorXd p0, SEXP W, double r) {
  if (Rf_isS4(W))
  {
    if(Rf_inherits(W, "dgCMatrix"))
      return mrwr_(p0, Rcpp::as<Eigen::SparseMatrix<double> >(W), r) ;
    Rcpp::stop("W has unknown class") ;
  }
  else
    return mrwr_(p0, Rcpp::as<Eigen::MatrixXd>(W), r) ;
}


