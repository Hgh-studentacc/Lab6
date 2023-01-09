#' Greedy Heuristic Algorithm for knapsack problem
#'
#' @param x A data frame with two elements, weights(w) and values(v)
#' @param W Knapsack capacity
#'
#' @return A list with optimal value and selected elements
#' @useDynLib Lab6
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#' @export

cppFunction(
  'List dist(NumericVector v, NumericVector w, int W, NumericVector names) {

  int n = v.size();  //so that our vector get optimal size

  int i = 0;
  int a = 0;
  int temp=0;
  NumericVector b(n);
  List L;

  while (temp < W) {
    a+=v[i];
    temp+=w[i];
    b.push_back(names[i]);
    i++;
  }


  L = List::create(Named("a") = a, Named("b") = names,Named("i") = i);
  return L;
  }'
)

greedy_knapsack_improved <- function(x, W) {
  stopifnot(is.data.frame(x) ||
              colnames(x) == c("w", "v") || is.data.frame(W))
  
  sorted_x = x[order(x$v / x$w, decreasing = TRUE),]
  #rewrite this while using c++
  # while (temp<W) {
  #   i=i+1
  #   a= sum(sorted_x$v[1:i])
  #   temp= sum(sorted_x$w[1:i])
  #   b[i]= rownames(sorted_x)[i]  }
  
  
  tempoD = dist(sorted_x$v, sorted_x$w, W, as.numeric(row.names(sorted_x)))
  result = list(value = round(tempoD$a - sorted_x$v[tempoD$i], 0),
                elements = as.numeric(tempoD$b[1:(tempoD$i - 1)]))
  return(result)
}


# n <- 2000
# knapsack_objects <-
#    data.frame(
#      w=sample(1:4000, size = n, replace = TRUE),
#      v=runif(n = n, 0, 10000)
#    )
#
#  greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack_improved(x = knapsack_objects[1:800,], W = 3500)
