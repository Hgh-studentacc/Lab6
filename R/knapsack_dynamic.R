#' Dynamic approach to solve knapsack problem
#'
#' @param x A data frame with two elements, weights(w) and values(v)
#' @param W Knapsack capacity
#'
#' @return A list with optimal value and selected elements
#' @export

knapsack_dynamic<- function(x, W){
  stopifnot(is.data.frame(x) || colnames(x) == c("w", "v"))
  n= nrow(x)
  m= matrix(0, ncol= W+1, nrow=n+1)

  weight=x[order(x[,1]), 1]
  value=x[order(x[,1]), 2]

  # print(m)
  for (i in 1:n+1){
    for(j in 1:W+1){
      if (i == 1 || j == 1) {
        #assigning 0 values
        m[i, j] <- 0
      }
      else if(weight[i-1]<j-1){
        m[i,j] = max(value[i-1]+m[i-1,j-weight[i-1]], m[i-1,j])
      }
      else if(weight[i-1]==j-1){
        m[i,j]= max(value[i-1]+0, m[i-1, j])
      }
      else{
        m[i,j]= m[i-1, j]
      }
    }
  }
  # print("here")
  ordered= order(x[,1])
  elements=c()
  i=1
  j=n+1
  k=W+1

  while(j>=2 && k>=1){
    if(m[j,k]>m[j-1,k]){
      elements[i]= ordered[j-1]
      k=k-weight[j-1]
      i=i+1

    }
    j=j-1
  }
  return(list(value= round(m[n+1, W+1], 0), elements= elements[order(elements)]))

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

# knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)

# How much time does it takes to run the algorithm for n = 500 objects?

# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = 500, replace = TRUE),
#     v=runif(n = 500, 0, 10000)
#   )
# #
# time_check <- function(){
#           a <-Sys.time()
#           knapsack_dynamic(x = knapsack_objects, W = 2000)
#           b <-Sys.time()
#           return(b-a)
#   }

# Time difference of 0.5400019 secs
