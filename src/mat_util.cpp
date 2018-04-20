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
// [[Rcpp::plugins(cpp11)]]
#include <cmath>
#include <vector>
#include <algorithm>


//' Column normalize a matrix, so that it is stochastic.
//'
//' @noRd
//' @param W  the adjacency matrix to be normalized
//' @return  returns the normalized matrix
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
Eigen::MatrixXd stoch_col_norm_(const Eigen::MatrixXd& W)
{
    Eigen::MatrixXd res(W.rows(), W.cols());
    Eigen::VectorXd colsums      = W.colwise().sum();
    const double    empt_col_val = 1.0 / W.cols();
    const double    zero_col     = 0.00001;
    for (unsigned int i = 0; i < W.cols(); ++i)
    {
        if ((W.col(i)).sum() <= zero_col)
            res.col(i).fill(empt_col_val);
        else
            res.col(i) = W.col(i) / colsums(i);
    }

    return res;
}


//' Calculate the Laplacian of a weighted matrix.
//'
//' @noRd
//' @param W  the adjacency matrix for which the Laplacian is calculated
//' @return  returns the Laplacian of a matrix
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
Eigen::MatrixXd laplacian_(const Eigen::MatrixXd& W)
{
    const int       P = W.rows();
    Eigen::MatrixXd res(W.rows(), W.cols());
    Eigen::VectorXd rowsums = W.rowwise().sum();
    for (int i = 0; i < P; ++i)
    {
        Rcpp::checkUserInterrupt();
        for (int j = 0; j < P; ++j)
        {
            if (i == j && rowsums[i] != 0)
                res(i, j) = 1 - (W(i, j) / rowsums(i));
            else if (i != j && W(i, j) != 0)
                res(i, j) = -W(i, j) / sqrt(rowsums(i) * rowsums(j));
            else
                res(i, j) = 0;
        }
    }
    return res;
}


double identity_(double d)
{
    return d;
}

//' Get the unweighted node degrees of a adjacency matrix.
//'
//' @noRd
//' @param W  the adjacency matrix to be normalized
//' @return  returns the node degrees as vectors.
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
std::vector<double> node_degrees_(const Eigen::MatrixXd& W)
{
    std::vector<double> node_degrees(W.rows());
    for (unsigned int i = 0; i < W.rows(); ++i)
    {
      node_degrees[i] = 0;
      for (unsigned int j = 0; j < W.cols(); ++j)
      {
        if (W(i, j) != 0) node_degrees[i]++;
      }
    }

    return node_degrees;
}


//' Normalize the hub bias in a matrix.
//'
//' @noRd
//' @param W  the adjacency matrix to be normalized
//' @return  returns the hub corrected matrix
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
Eigen::MatrixXd hub_normalize_(const Eigen::MatrixXd& W)
{
    auto &fc(identity_);
    Eigen::MatrixXd res = Eigen::MatrixXd::Constant(W.rows(), W.cols(), 0.0);
    std::vector<double> node_degrees = node_degrees_(W);

    for (unsigned int i = 0; i < W.rows(); ++i)
    {
      for (unsigned int j = 0; j < W.cols(); ++j)
      {
          if (W(i, j) != 0)
          {
              double mh = fc(node_degrees[i] / node_degrees[j]);
              res(i, j) = std::min(1.0, mh) / node_degrees[i];
          }
      }
    }

    return res;
}
