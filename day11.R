library(stringr)
library(dplyr)
library(gmp)

day11 <- readLines("Advent of Code/day11.txt")

day11 <- lapply(day11, function(x) unlist(str_split(x, ": ")))
day11_df <- do.call(rbind, lapply(day11, function(x){
  str1 <- x[[1]]
  str2 <- unlist(str_split(x[[2]], " "))
  data.frame(start=str1, end=str2)
}))

#starts <- day11_df |> filter(day11_df$start=="you") |> rename(end1=end)

is.circular <- function(vec){
  length(vec) > length(unique(vec))
}

build_connections <- function(hookups, start_str, end_str){
  n_cons <- 1
  starts <- hookups |> filter(start==start_str) |> rename(end1=end)
  starts$n <- 1
  while(nrow(starts) > 0){
    n_cons <- n_cons+1
    merge_df <- hookups
    names(merge_df) <- c(paste0("end", n_cons-1), paste0("end", n_cons))
    starts <- merge(starts, merge_df)
    conns_made <- which(starts[[paste0("end", n_cons)]]==end_str)
    if(length(conns_made) > 0){
      conns <- starts[conns_made,]
      starts <- starts[-conns_made,]
      if(!exists('connections')){
        connections <- conns
      } else {
        connections <- bind_rows(connections, conns)
      }
    }
    #circular_cons <- sapply(1:nrow(starts), function(x) is.circular(starts[x,]))
   #if(any(circular_cons)) starts <- starts[-which(circular_cons),]
    starts <- starts |> select(c(start, ends_with(as.character(n_cons)), n)) |> group_by_if(is.character) |> summarize(n=sum(n), .groups="drop")
  }
  if(exists("connections")) return(connections) else return(NULL)
}

#part1
p1_conns <- build_connections(day11_df, "you", "out")

#part2
p2_conns1 <- build_connections(day11_df, "fft","dac")
p2_conns2 <- build_connections(day11_df, "svr", "fft")
p2_conns3 <- build_connections(day11_df, "dac", "fft")
p2_conns4 <- build_connections(day11_df, "dac", "out")
p2_conns5 <- build_connections(day11_df, "fft", "out")

#no connections from dac -> fft means we drop srv -> dac & fft -> out
as.bigz(sum(p2_conns2$n))*as.bigz(sum(p2_conns1$n))*as.bigz(sum(p2_conns4$n))
