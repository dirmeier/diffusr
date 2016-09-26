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
#include <vector>

//' Do a Markon random walk (with restart) on an column-normalised adjacency matrix.
//'
//' @noRd
//' @param p0  the staring distribution
//' @param W  the column normalized adjacency matrix
//' @param r  restart probability
//' @return  returns the stationary distribution p_inf
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export(name=".mrwr.cpp")]]
Eigen::VectorXd mrwr_(const Eigen::VectorXd& p0,
                      const Eigen::MatrixXd& W,
                      const double r)
{
  Eigen::VectorXd pt = p0;
  Eigen::VectorXd pold;
  const double thresh = .00000001;
  do
  {
    pold = pt;
    pt = (1  - r) * W * pold + r * p0;
  }
  while ((pt - pold).norm() > thresh);
  return pt;
}




