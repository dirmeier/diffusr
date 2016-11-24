/** diffusr: network diffusion algorithms in R
 *
 * Copyright (C) 2016 Simon Dirmeier
 * @author Simon Dirmeier
 * @email simon.dirmeier@bsse.ethz.ch
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

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

//' Do graph diffusion using an insulated heat kernel.
//'
//' @noRd
//' @param v0  the starting heat
//' @param W  the column normalized adjacency matrix
//' @param b  restart probability
//' @return  returns the stationary distribution p_inf
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export(name=".heat.diffusion.cpp")]]
Eigen::VectorXd heat_diffusion_(const Eigen::VectorXd& v0,
                                const Eigen::MatrixXd& W,
                                const double b)
{
  const int m = W.rows();
  Eigen::VectorXd vt = v0;
  Eigen::VectorXd vold;
  Eigen::MatrixXd I = Eigen::MatrixXd::Identity(m, m);
  Eigen::MatrixXd F = b * (I  - (1 - b) * W).inverse();
  const double thresh = .00000001;
  const int niter = 100000;
  int iter = 0;
  do
  {
    vold = vt;
    vt = F * vold;
  }
  while ((vt - vold).norm() > thresh && iter++ < niter);
  return vt;
}
