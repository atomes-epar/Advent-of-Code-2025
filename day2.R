library(stringr)
day2 <- readLines("day2.txt")
day2 <- str_split(day2, ",") |> unlist()
#comps <- lapply(day2, FUN=function(x){mapply(str_split(x, "-"), as.numeric)})
get_silly_id <- function(silly_prefix){
  paste0(silly_prefix, silly_prefix) |> as.numeric()
}

get_silly_id2 <- function(silly_prefix, oom){
  reps <- ceiling(oom/nchar(as.character(silly_prefix)))
  if(reps>1){
  paste0(rep(silly_prefix, (reps)), collapse="") |> as.numeric()
  } else {
    return(Inf)
  }
}

running_total <- 0
silly_ids <- list()
for(i in 1:length(day2)){
  silly_list <- vector()
comps <- lapply(str_split(day2[[i]], "-"), as.numeric) |> unlist()
startpos <- as.numeric(comps[[1]])
endpos <- as.numeric(comps[[2]])
oom <- 0
while(startpos > 10){
  startpos <- startpos/10
  oom <- oom+1
}
if(oom %%2 == 0){
  startpos <- 10^(oom+1)
} else { 
  startpos <- startpos * 10^(oom)
}
if(startpos < endpos){

silly_prefix <- as.character(startpos)
silly_prefix <- str_sub(startpos, 1, nchar(silly_prefix)/2) |> as.numeric()  
silly_id <- get_silly_id(silly_prefix)
while(silly_id < startpos){
  silly_prefix <- silly_prefix+1
  silly_id <- get_silly_id(silly_prefix)
}

while(silly_id <= endpos){
  running_total <- running_total+silly_id
  silly_list <- c(silly_list, silly_id)
  silly_prefix <- silly_prefix+1
  silly_id <- get_silly_id(silly_prefix)
}
}
silly_ids[[i]] <- c(silly_list)
}

#part 2
get_silly_ids <- function(range){
  range <- str_split(range, "-") |> unlist() |> as.numeric()
  search_list <- seq(as.numeric(range[[1]]), as.numeric(range[[2]])) |> as.character()
  search_list <- search_list[str_detect(search_list, "(^[0-9]+?)\\1+$")]
  return(sapply(search_list, as.numeric))
}

result2 <- lapply(day2, get_silly_ids) |> unlist()
gmp::as.bigz(Reduce(`+`, result2))
