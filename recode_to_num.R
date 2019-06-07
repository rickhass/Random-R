#function to change Likert statements into numeric variable
recode_to_num <- function(data, vars, statements, values){ 
  # data is the name of the dataframe
  # vars is a vector of names as strings
  # statements is a vector of the likert statements / semantic differentials in order from lowest to highest
  # values are the corresponding numeric values. Must be the same size as 
  d <- subset(data, select = vars)
  # cannot use original data as it will be "char" so make a matrix the same size
  output <- matrix(data = NA, nrow = nrow(d), ncol = ncol(d))
  # check for factors, if present, convert to character
  for (i in 1:ncol(d)){
    if (is.factor(d[,i])){
      d[,i] <- as.character(d[,i])
    }
  }
  for (i in 1:nrow(d)){
    for (j in 1:ncol(d)){
      x <- match(tolower(d[i,j]), statements)
      if (is.na(x)){
        output[i,j] <- NA
      } else {
        output[i,j] <- values[x]
      }
    }
  }
  output
}