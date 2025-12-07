library(readr)
day6 <- read_table("day6.txt", col_names = FALSE)
day6 <- day6[,-1001] #Last column is empty
result <- sum(
  sapply(1:ncol(day6), FUN=function(x){
    col <- day6[,x] |> unlist()
    op <- if(col[[5]]=="+") `+` else `*`
    Reduce(op, as.numeric(col[1:4]))
  })
)

#part2

day6 <- readLines("day6.txt")
day6 <- day6[,-1001] #Last column is empty
op_pos <- str_locate_all(day6[[5]], "[+*]")[[1]][,1] |> unlist()
result <- sum(
  result <-  sapply(1:length(op_pos), function(x){
  startpos <- op_pos[[x]]
  endpos <- if(x==length(op_pos)) nchar(day6[[5]]) else op_pos[[x+1]]-2
  each_num <- sapply(startpos:endpos, function(y){
    paste(sapply(1:4, function(z) str_sub(day6[[z]], y, y)), collapse="") |> as.numeric()
    
  })
  op <- if(str_sub(day6[[5]], startpos, startpos)=="+") `+` else `*`
  Reduce(op, each_num)
})
) |> as.bigz()
