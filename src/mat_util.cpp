/** diffusr: network diffusion algorithms in R
 *
 * Copyright (C) 2016 Simon Dirmeier
 *
 * This file is part of diffusr.
 *
 * diffusr is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * diffusr is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with diffusr. If not, see <http://www.gnu.org/licenses/>.
 */

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
// [[Rcpp::export(name=".stoch.col.norm.cpp")]]
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

