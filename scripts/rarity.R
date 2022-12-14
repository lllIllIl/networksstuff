#this function takes as "x" a table of presence/absence data where rows are
#sites and columns are artifact types. It outputs a table of each artifact
#with corresponding "spread" and "inverse" rankings, with "inverse" serving
#as its rarity.
rarity <- function(x){
  degun <- as.data.frame(matrix(NA, 2, ncol(x)))
  colnames(degun) <- colnames(x)
  for (i in seq_len(ncol(x))){
    degun[1,i] <- sum(x[,i])
  }
  degnorm <- as.data.frame(degun)
  for (i in seq_len(ncol(x))){
    degnorm[1,i] <- (degun[1,i]/nrow(x))
  }
  for (i in seq_len(ncol(x))){
    degnorm[2,i] <- abs(degnorm[1,i]-1)
  }
  degout <- as.data.frame(t(degnorm))
  colnames(degout) <- c("Spread","Inverse")
  return(degout)
}