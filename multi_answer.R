multi_answer <- function(x, correct){
  # x is a character vector
  output <- vector("numeric", length = length(x))
  for (i in 1:length(x)){
    a <- strsplit(x[i], split = ",")
    
    if (length(a[[1]]) > 0){
      points <- vector("numeric", length = length(correct))
      for (j in 1:length(correct)){
        if (length(grep(correct[j], a[[1]])) > 0){
          points[j] <- 1
        } else {
          points[j] <- 0
        }
        output[i] <- sum(points)
      }
    }
    else {
      output[i] <- 0
    }
  }
  output
}