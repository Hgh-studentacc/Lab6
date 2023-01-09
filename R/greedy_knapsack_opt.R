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

greedy_knapsack_improved<- function(x, W){
  stopifnot(is.data.frame(x) || colnames(x) == c("w", "v")|| is.data.frame(W))

  sorted_x= x[order(x$v/x$w, decreasing = TRUE), ]
  i=0
  temp= 0
  a= vector()
  b= vector()
#rewrite this while using c++
  while (temp<W) {
    i=i+1
    a= sum(sorted_x$v[1:i])
    temp= sum(sorted_x$w[1:i])
    b[i]= rownames(sorted_x)[i]

  }

  result= list(value= round(a-sorted_x$v[i],0), elements= as.numeric(b[1:(i-1)]))
  return(result)
}


# RNGversion(min(as.character(getRversion()),"3.5.3"))
#
# ##old sampler used for backward compatibility
# ## suppressWarnings() can be used so that the above warning is not displayed
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )

# greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

# How much time does it takes to run the algorithm for n = 1000000 objects?

# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = 1000000, replace = TRUE),
#     v=runif(n = 1000000, 0, 10000)
#   )
#
# time_check <- function(){
#           a <-Sys.time()
#           greedy_knapsack(x = knapsack_objects, W = 2000)
#           b <-Sys.time()
#           return(b-a)
#   }

# Time difference of 0.1925881 secs

