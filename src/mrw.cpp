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
#include <vector>

//' Do a Markon random walk (with restart) on an column-normalised adjacency
// matrix.
//'
//' @noRd
//' @param p0  matrix of starting distribution
//' @param W  the column normalized adjacency matrix
//' @param r  restart probability
//' @param thresh  threshold to break as soon as new stationary distribution
//   converges to the stationary distribution of the previous timepoint
//' @param niter  maximum number of iterations for the chain
//' @param do_analytical  boolean if the stationary distribution shall be
//'  computed solving the analytical solution or iteratively
//' @return  returns the matrix of stationary distributions p_inf
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
Eigen::VectorXd mrwr_(const Eigen::MatrixXd& p0,
                      const Eigen::MatrixXd& W,
                      const double r,
                      const double thresh,
                      const int niter,
                      const bool do_analytical))
{
    Eigen::MatrixXd pt;
    if (!do_analytical)
    {
      Eigen::MatrixXd pt = p0;
      Eigen::MatrixXd pold;
      int iter   = 0;
      do
      {
          if (iter % 25 == 0)
              Rcpp::checkUserInterrupt();
          pold = pt;
          pt   = (1 - r) * W * pold + r * p0;
      } while ((pt - pold).norm() > thresh && iter++ < niter);
    }
    else
    {
      Eigen::MatrixXd T  = r * (I - (1 - r) * W).inverse();
      pt = T %*% p0;
    }

    return pt;
}
