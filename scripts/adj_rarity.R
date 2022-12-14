#This function takes a presence/absence table "x" with rows being sites and
#columns being artifacts. It produces an identically structured table with
#each instance of binary data being converted to continuous based on the 
#percieved rarity of an artifactdetermined by how few sites it is connected to.
adj_rarity <- function(x){
  degnorm <- as.data.frame(matrix(NA,nrow(x),ncol(x)))
  colnames(degnorm) <- colnames(x)
  for (i in seq_len(ncol(x))){
    degnorm[,i] <- sum(x[,i])
  }
  for (i in seq_len(ncol(x))){
    degnorm[,i] <- (abs(degnorm[1,i]/nrow(x)-1))
  }
  output <- as.data.frame((x) * (degnorm))
  return (output)