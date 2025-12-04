library(stringr)
day4 <- readLines("C:/Users/altomes/Downloads/day4.txt")
day4_num <- sapply(day4, FUN=function(x){
  str <- str_replace_all(x, "\\.", "0")
  str <- str_replace_all(str, "@", "1")
  return(sapply(strsplit(str, ""), as.numeric))
}, USE.NAMES = F)


prefix_sums <- matrix(data=rep(0, (nrow(day4_num)+1)*(ncol(day4_num)+1)), nrow=nrow(day4_num)+1, ncol=ncol(day4_num)+1)

for(i in 2:nrow(prefix_sums)) {
  for(j in 2:ncol(prefix_sums)){
    prefix_sums[i,j] <- prefix_sums[i-1, j] + prefix_sums[i, j-1] - prefix_sums[i-1,j-1] + day4_num[i-1,j-1]
  }
}

blocksums <- function(mat, k, prefixes) {
  results <- matrix(nrow=nrow(mat), ncol=ncol(mat))
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      top_row <- max(i-k, 1)
      left_col <- max(j-k, 1)
      bottom_row <- min(nrow(mat), i+k)
      right_col <- min(ncol(mat), j+k)
      
      total_rolls <- prefixes[bottom_row+1, right_col+1] - prefixes[top_row, right_col+1] - 
        prefixes[bottom_row+1, left_col] + prefixes[top_row, left_col]
      #Modification for simpler solution.
      #results[i,j] <- if(total_rolls<5 & mat[i,j]==1) 1 else 0
      results[i,j] <- total_rolls
    }
  }
  return(results)
}

result <- blocksums(day4_num, 1, prefix_sums)
sum(result)


#part 2
compute_prefixes <- function(mat){
  prefix_sums <- matrix(data=rep(0, (nrow(mat)+1)*(ncol(mat)+1)), nrow=nrow(mat)+1, ncol=ncol(mat)+1)
  for(i in 2:nrow(prefix_sums)) {
    for(j in 2:ncol(prefix_sums)){
      prefix_sums[i,j] <- prefix_sums[i-1, j] + prefix_sums[i, j-1] - prefix_sums[i-1,j-1] + mat[i-1,j-1]
    }
  }
  return(prefix_sums)
}

remove_rolls <- function(mat, k) {
  rolls_left <- T
  while(rolls_left){
    new_mat <- matrix(nrow=nrow(mat), ncol=ncol(mat))
    prefixes <- compute_prefixes(mat)
    for(i in 1:nrow(mat)){
      for(j in 1:ncol(mat)){
        if(mat[i,j]==0) {
          new_mat[i,j] <- 0
        } else {
          top_row <- max(i-k, 1)
          left_col <- max(j-k, 1)
          bottom_row <- min(nrow(mat), i+k)
          right_col <- min(ncol(mat), j+k)
          
          total_rolls <- prefixes[bottom_row+1, right_col+1] - prefixes[top_row, right_col+1] - 
            prefixes[bottom_row+1, left_col] + prefixes[top_row, left_col]
          new_mat[i,j] <- if(total_rolls<5) 0 else 1
        }
      }
    }
    if(sum(new_mat)==sum(mat)){
      rolls_left <- F
    } else {
      mat <- new_mat
    }
  }
  return(mat)
}

removed_rolls <- remove_rolls(day4_num, 1)

#Noodling
remove_rolls2 <- function(mat, k){
  rolls_left <- T
  frames <- matrix(data=rep(0, (nrow(mat))*(ncol(mat))), nrow=nrow(mat), ncol=ncol(mat))
  while(rolls_left) {
    frames <- frames+mat
    matsum <- sum(mat)
  result <- blocksums(mat, k, compute_prefixes(mat))*mat
  mat <- ifelse(result < 5, 0, 1)
  if(sum(mat) == matsum){
    rolls_left <- F
  }
  }
  return(list(mat=mat, frames=frames))
}

removed_rolls2 <- remove_rolls2(day4_num, 1)

frames <- removed_rolls2$frames
frames.df <- reshape2::melt(frames, c("x", "y"), value.name = "z")
startdf <- reshape2::melt(day4_num, c("x", "y"), value.name = "z")

#How to do it without recursion.
removed_rolls3 <- function(mat, k) {
  result <- blocksums(mat, k, compute_prefixes(mat))*mat
  result <- ifelse(result<5, 0,1)
  result2 <- blocksums(result, compute_prefixes(result))*result
  result2 <- ifelse(result2 < 4, 0, 1)
  return(result2)
}

