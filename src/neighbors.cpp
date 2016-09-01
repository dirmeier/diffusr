/**
 * @author Simon Dirmeier
 * @email simon.dirmeier@bsse.ethz.ch
 */

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppEigen.h>
#include <vector>
#include <set>
#include <cstdlib>
#ifdef _OPENMP
#include <omp.h>
#endif



void add_neighbors_(std::set<int>& nodes,
                    const int row_idx,
                    const int curr_depth,
                    const int k,
                    const Rcpp::NumericMatrix& W)
{

}


//' Find the closest neighbors of a group of nodes in a graph.
//'
//' @noRd
//' @param node_idxs  the staring distribution
//' @param W  adjacency matrix
//' @param k  the depth of the nearest neighbor search
//' @param use_edge_weights  boolean flags if the edge weights should be considered when doing nearest neighbor lookup
//' @return  returns a list of nearest neighbors for every node idxs given in <emph>node_idxs</emph>
// [[Rcpp::export]]
Rcpp::List do_neighbors(const Rcpp::IntegerVector& node_idxs,
                        const Rcpp::NumericMatrix& W,
                        const int k,
                        const bool use_edge_weights)
{
  uint32_t len = static_cast<uint32_t>(node_idxs.size());
  std::vector< std::set<int> > neighbors(len);
  const int nrow = W.nrow();
  const int ncol = W.ncol();
  # pragma omp parallel for
  for (uint32_t i = 0; i < len, ++i)
  {
    const int node_idx = node_idxs[i];
    std::set<int> node_neighbors;
    neighbors.push_back(node_neighbors);
    add_neighbors_(node_neighbors, i, 0, k, W);
  }
  return Rcpp::wrap(neighbors);
}


void add_neighbors_(std::set<int>& nodes,
                    const int row_idx,
                    const int curr_depth,
                    const int k,
                    const Rcpp::NumericMatrix& W)
{

}

