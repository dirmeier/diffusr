/**
 * @author Simon Dirmeier
 * @email simon.dirmeier@bsse.ethz.ch
 */

// [[Rcpp::plugins(cpp11)]]
#include <vector>
#include <set>
#include <cstdlib>
#ifdef _OPENMP
#include <omp.h>
#endif

void add_neighbors_(std::set<int>& nodes,
                    std::vector<bool>& visited,
                    const int row_idx,
                    const int curr_depth,
                    const int k,
                    const std::vector< std::vector<int> >& adj)
{
  visited[row_idx] = true;
  if (curr_depth < k)
  {
    for (uint32_t i = 0; i < adj[row_idx].size(); ++i)
    {
      int idx = adj[row_idx][i];
      if (!visited[idx])
      {
        nodes.insert(idx);
        add_neighbors_(nodes, visited, idx, curr_depth + 1, k, adj);
      }
    }
  }
}

std::vector< std::vector<int> > init_adj_list_(const Rcpp::NumericMatrix& W)
{
  std::vector< std::vector<int> > adj(W.nrow());
  for (int i = 0; i < W.nrow(); ++i)
  {
    std::vector<int> neighs;
    for (int j = 0; j < W.ncol(); ++j)
        if (W(i, j)) neighs.push_back(j);
    adj[i] = neighs;
  }
  for (int i = 0; i < adj.size(); ++i)
  {
    Rcpp::Rcout <<  "Idx " << i << " -> ";
    for (int j = 0; j < adj[i].size(); ++j)
      Rcpp::Rcout << adj[i][j] << " ";
    Rcpp::Rcout <<  "\n";
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
// [[Rcpp::export]]
Rcpp::List do_neighbors(std::vector<int>& node_idxs,
                        const Rcpp::NumericMatrix& W,
                        const int k,
                        const bool use_edge_weights)
{
  // number of idxs given
  uint32_t len = static_cast<uint32_t>(node_idxs.size());
  // neighbors for every node
  std::vector< std::set<int> > neighbors(len);
  // setup adjacency list
  std::vector<std::vector<int> > adj = init_adj_list_(W);
  // parallelize node search
  # pragma omp parallel for
  for (uint32_t i = 0; i < len; ++i)
  {
    // substract one, cause R was one-based
    const int node_idx = node_idxs[i] - 1;
    // neighbors of current node
    std::set<int> node_neighbors;
    node_neighbors.insert(100);
    neighbors[i] =  node_neighbors;
    // set visited matrix
    std::vector<bool> visited(W.nrow(), false);
    // recursively add neighbors
    add_neighbors_(node_neighbors, visited, node_idx, 0, k, adj);
  }
  return Rcpp::wrap(neighbors);
}

