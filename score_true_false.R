# Function for scoring T/F items
score_true_false <- function(data, vars, answers){
  d <- data[vars]
  output <- matrix(0, nrow(d), ncol(d))
  for(i in 1:nrow(d)){
    for (j in 1:ncol(d)){
      if (d[i,j] == answers[j]){
        output[i,j] <- 1
      } else {
        output[i,j] <- 0
      }
    }
  }
  output
}