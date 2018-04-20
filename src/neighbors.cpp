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

// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <vector>
#include <set>
#include <cstdlib>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <queue>

struct distance_comparator
{
    bool operator()(std::pair<int, double>& lhs, std::pair<int, double>& rhs)
    {
        return lhs.second < rhs.second;
    }
};

bool equals(const double val, const double cmp, const double delta)
{
    return val <= cmp + delta && val >= cmp - delta;
}

std::priority_queue<std::pair<int, double>,
                    std::vector<std::pair<int, double>>,
                    distance_comparator>
  init_queue(const Rcpp::NumericMatrix& W, const int source)
{
    std::priority_queue<std::pair<int, double>,
                        std::vector<std::pair<int, double>>,
                        distance_comparator> queue;

    for (int i = 0; i < W.cols(); ++i)
    {
        if (i != source && W(source, i) > 0)
        {
            queue.push(std::make_pair(i, W(source, i)));
        }
    }

    return queue;
}

std::vector<std::pair<int, double>> current_neighbors(
  std::priority_queue<std::pair<int, double>,
                      std::vector<std::pair<int, double>>,
                      distance_comparator>& queue,
  std::vector<uint8_t>& visited)
{

    // get the nearest neighbor
    std::pair<int, double> cn = queue.top();
    // remove from queue
    queue.pop();

    std::vector<std::pair<int, double>> curr_nei;
    // add the node itself to the list, since we iterate over the list later
    if (!visited[cn.first])
    {
        curr_nei.push_back(cn);
    }

    // add neighbors that are as close as the first neighbor `cn`
    while (queue.size() && equals(queue.top().second, cn.second, .001))
    {
        std::pair<int, double> nn = queue.top();
        if (!visited[nn.first])
            curr_nei.push_back(nn);
        queue.pop();
    }

    return curr_nei;
}

void add_neighbor_to_queue(
  std::priority_queue<std::pair<int, double>,
                    std::vector<std::pair<int, double>>,
                    distance_comparator>& queue,
  const Rcpp::NumericMatrix& W,
  const std::pair<int, double>& cn)
{
    for (int i = 0; i < W.cols(); ++i)
    {
        if (i != cn.first && W(cn.first, i) > 0)
        {
            queue.push(std::make_pair(i, W(cn.first, i)));
        }
    }
}

void nearest_neighbor_dijkstra_(std::set<int>&             nei,
                                const int                  source,
                                const int                  max_depth,
                                const Rcpp::NumericMatrix& W)
{
    // use a priority queue to quickly extract nearest neighbors
    std::priority_queue<std::pair<int, double>,
                        std::vector<std::pair<int, double>>,
                        distance_comparator>
      queue = init_queue(W, source);
    // boolean vector if nodes have already been visited in the BFS
    std::vector<uint8_t> visited(W.rows(), false);

    // traverse graph until a certain depth is reached
    int r = 1;
    do
    {
        Rcpp::checkUserInterrupt();
        // list of the nearest neighbors
        std::vector<std::pair<int, double>> curr_nei =
          current_neighbors(queue, visited);
        // iterate over current nearest neighbors and add them to the results
        // list
        for (const std::pair<int, double>& cn : curr_nei)
        {
            if (visited[cn.first] && cn.first == source)
            {
                continue;
            }
            else
            {
                visited[cn.first] = true;
            }
            // add as neighbor with index + 1, cause neighbors are indexe
            // starting from 1 in R
            nei.insert(cn.first + 1);
            // add current node to priority queue
            add_neighbor_to_queue(queue, W, cn);
        }
    } while (r++ < max_depth && queue.size());
}

//' Find the closest neighbors of a group of nodes in a graph.
//'
//' @noRd
//' @param node_idxs  the staring distribution
//' @param W  adjacency matrix
//' @param k  the depth of the nearest neighbor search
//' @return  returns a list of nearest neighbors for every node idxs given in
//'  <emph>node_idxs</emph>
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
Rcpp::List neighbors_(const Rcpp::IntegerVector& node_idxs,
                      const Rcpp::NumericMatrix& W,
                      const int                  k)
{
    // number of idxs given
    const uint32_t len = static_cast<uint32_t>(node_idxs.size());
    // neighbors for every node
    std::vector<std::set<int>> neighbors(len);

    // parallelize node search
    #pragma omp parallel for
    for (uint32_t i = 0; i < len; ++i)
    {
        // substract one, cause R was one-based
        const int node_idx = static_cast<int>(node_idxs[i]) - 1;
        // neighbors of current node
        neighbors[i] = std::set<int>();
        // run disjkstra until k neighbors are found
        nearest_neighbor_dijkstra_(neighbors[i], node_idx, k, W);
    }

    return Rcpp::wrap(neighbors);
}
