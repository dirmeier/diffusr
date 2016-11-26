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

//' Do graph diffusion using an heat diffusion on a Laplacian.
//'
//' @noRd
//' @param v0  the starting heat
//' @param W  the normalized Laplacian of the matrix
//' @param t  time for which geat is measured
//' @return  returns the stationary distribution p_inf
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export(name=".laplacian.heat.diffusion.cpp")]]
Eigen::VectorXd laplacian_diffusion_(const Eigen::VectorXd& v0,
                                     const Eigen::MatrixXd& W,
                                     const double t)
{



  sl = eigen(t(L))
  D = rev(sl$values)
  V = sl$vectors[,400:1]
  C0 = matrix(0, N, N)
  C0[2:5, 2:5] = 5;
  C0[10:15, 10:15] = 10;
  C0[2:5, 8:13] = 7;
  plotrix::color2D.matplot(C0,show.legend = TRUE, axes = TRUE,
                           xlab = "", ylab = "",
                           extremes = c("blue", "red"))
    C0V <-  t(V) %*%  as.vector(C0)
    for (t in seq(0, 5, by=.05))
    {
      Phi = (C0V) * (exp(-D * t))
      Phi = V %*% (Phi)
      Phi = matrix(Phi, N, N);
      plotrix::color2D.matplot(Phi,show.legend = TRUE, axes = F,
                               xlab = "", ylab = "",
                               extremes = c("blue", "red"))
        Sys.sleep(.05)
    }


}
