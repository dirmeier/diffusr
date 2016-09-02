/**
 * @author Simon Dirmeier
 * @email simon.dirmeier@bsse.ethz.ch
 */

// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <vector>
#include <set>
#include <cstdlib>
#ifdef _OPENMP
#include <omp.h>
#endif

void add_neighbors_(std::set<int>& nodes,
                    std::vector<uint8_t>& visited,
                    const uint32_t row_idx,
                    const int curr_depth,
                    const int max_depth,
                    const std::vector<std::vector<int>>& adj)
{
  visited[row_idx] = true;
  if (curr_depth < max_depth)
  {
    for (uint32_t i = 0; i < adj[row_idx].size(); ++i)
    {
      const int idx = adj[row_idx][i];
      if (!visited[idx])
      {
        nodes.insert(idx + 1);
        add_neighbors_(nodes, visited, idx, curr_depth + 1, max_depth, adj);
      }
    }
  }
}

std::vector<std::vector<int>> init_adj_list_(const Rcpp::NumericMatrix& W)
{
  std::vector<std::vector<int>> adj(W.nrow());
  #pragma omp parallel for
  for (int i = 0; i < W.nrow(); ++i)
  {
    std::vector<int> neighs;
    for (int j = 0; j < W.ncol(); ++j)
    {
        if (i != j && W(i, j))
        {
          neighs.push_back(j);
        }
    }
    adj[i] = neighs;
  }
  return adj;
}

//' Find the closest neighbors of a group of nodes in a graph.
//'
//' @noRd
//' @param node_idxs  the staring distribution
//' @param W  adjacency matrix
//' @param k  the depth of the nearest neighbor search
//' @param use_edge_weights  boolean flags if the edge weights should be considered when doing nearest neighbor lookup
//' @return  returns a list of nearest neighbors for every node idxs given in <emph>node_idxs</emph>
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export(name=".neighbors_cpp")]]
Rcpp::List neighbors_(const Rcpp::IntegerVector& node_idxs,
                      const Rcpp::NumericMatrix& W,
                      const int k,
                      const bool use_edge_weights)
{
  // number of idxs given
  uint32_t len = static_cast<uint32_t>(node_idxs.size());
  // neighbors for every node
  std::vector<std::set<int>> neighbors(len);
  // setup adjacency list
  std::vector<std::vector<int>> adj = init_adj_list_(W);
  // parallelize node search
  #pragma omp parallel for
  for (uint32_t i = 0; i < len; ++i)
  {
    // substract one, cause R was one-based
    const uint32_t node_idx = node_idxs[i] - 1;
    // neighbors of current node
    neighbors[i] = std::set<int>();
    // set visited matrix
    std::vector<bool> visited(W.nrow(), false);
    // recursively add neighbors
    add_neighbors_(neighbors[i], visited, node_idx, 0, k, adj);
  }
  return Rcpp::wrap(neighbors);
}

