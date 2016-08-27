// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// do_stoch_col_norm
SEXP do_stoch_col_norm(Eigen::MatrixXd W);
RcppExport SEXP diffusr_do_stoch_col_norm(SEXP WSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type W(WSEXP);
    __result = Rcpp::wrap(do_stoch_col_norm(W));
    return __result;
END_RCPP
}
// do_mrwr
Eigen::VectorXd do_mrwr(Eigen::VectorXd p0, SEXP W, double r);
RcppExport SEXP diffusr_do_mrwr(SEXP p0SEXP, SEXP WSEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Eigen::VectorXd >::type p0(p0SEXP);
    Rcpp::traits::input_parameter< SEXP >::type W(WSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    __result = Rcpp::wrap(do_mrwr(p0, W, r));
    return __result;
END_RCPP
}
