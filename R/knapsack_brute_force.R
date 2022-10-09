#' Brute Force method(exhaustive search) to solve knapsack problem
#'
#' @param x A data frame with two elements, weights(w) and values(v)
#' @param W Knapsack capacity
#' @param methhod fast Not_so_fast
#' @return A list with optimal value and selected elements
#' @export



brute_force_knapsack=function(x,W,methhod="Not_so_fast"){
  stopifnot(is.data.frame(x) || colnames(x) == c("w", "v")|| is.data.frame(W))

  MrHankMufflin = list()
  if (methhod=="fast"){
    url1="https://raw.githubusercontent.com/Hgh-studentacc/personal_uploads/main/knapsack_brute_force.cpp"
    httr::GET(url1, httr::write_disk(tfile1 <- tempfile(fileext = ".cpp")))
    Rcpp::sourceCpp(tfile1)
    #Rcpp::sourceCpp(system.file("Extra/knapsack_brute_force.cpp", package="Lab6"))
    return(bruteforce_knapsack(x[[1]],x[[2]],W,nrow(x[1])))
  } else {
    W_max=W
    max_P=0
    items=list()
    m=length(x[[1]])

    for (i in 1:2^m-1){
      bin = intToBits(i)
      profit = 0
      weight_iter = 0

      jvec=c()
      for (j in 1:length(bin)){
        if (bin[j]==1){
          weight_iter=weight_iter+x$w[j]
          if(weight_iter>W_max){
            profit=0
            break
          }
          profit=profit+x$v[j]
          jvec=append(jvec,j)
        }

      }

      if ((max_P<profit) & (W_max>=weight_iter)){
        items[i]=i

        max_P=profit
        #print(jvec)
        rollblocker=jvec
      }
    }
  }


  MrHankMufflin$value = round(max_P)
  MrHankMufflin$elements=rollblocker
  return(MrHankMufflin)
}


