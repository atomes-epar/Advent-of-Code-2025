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
running_total <- 0
silly_ids2 <- list()
for(i in 1:length(day2)){
  comps <- lapply(str_split(day2[[i]], "-"), as.numeric) |> unlist()
  silly_list <- vector()
  startpos <- as.numeric(comps[[1]])
  endpos <- as.numeric(comps[[2]])
  startpos_oom <- startpos
  endpos_oom <- endpos
  oom <- 1
  oom_min <- 0
  while(startpos_oom > 10){
    startpos_oom <- startpos_oom/10
    oom_min <- oom_min+1
  }
  while(endpos_oom > 10) {
    endpos_oom <- endpos_oom/10
    oom <- oom+1
  }
  if(oom > 0) {
  for(j in 1:ceiling(oom/2)) {
    silly_prefix <- if(nchar(floor(startpos_oom)) == 1) 1 else floor(startpos_oom)
    silly_id <- get_silly_id2(silly_prefix, oom)
    while(silly_id < startpos){
      silly_prefix <- silly_prefix+1
      silly_id <- get_silly_id2(silly_prefix, oom)
    }
    while(silly_id <= endpos){
      if(!any(silly_id == silly_list) & silly_id >= startpos & nchar(silly_id) > 1){ #Figure out how to make this less kludgy?
      running_total <- running_total + silly_id
      silly_list <- c(silly_list, silly_id)
      }
      silly_prefix <- silly_prefix+1
      silly_id <- get_silly_id2(silly_prefix, oom)
    }
    startpos_oom <- startpos_oom*10
    }
  }
  silly_ids2[[i]] <- silly_list
}

running_total <- 0
silly_ids2 <- list()
for(i in 1:length(day2)){
  comps <- lapply(str_split(day2[[i]], "-"), as.numeric) |> unlist()
  silly_list <- vector()
  startpos <- as.numeric(comps[[1]])
  endpos <- as.numeric(comps[[2]])
  startpos_nchar <- nchar(startpos)
  endpos_nchar <- nchar(endpos)
  
  for(j in 1:ceiling(endpos_nchar/2)){
    if(j==1){
      silly_prefix <- 1
    } else {
    silly_prefix <- str_sub(comps[[1]], 1, j) |> as.integer()
    }
    while(nchar(silly_prefix) == j) {
      for(k in startpos_nchar:endpos_nchar){
        if(k>1 & nchar(silly_prefix) < k){
          silly_id <- paste0(rep(silly_prefix, ceiling(k/nchar(silly_prefix))), collapse="") |> as.numeric()
          if(silly_id >= startpos & silly_id <= endpos & !any(silly_id==silly_list)){
            silly_list <- c(silly_list, silly_id)
            running_total <- running_total+silly_id
          }
        }
      }
      silly_prefix <- silly_prefix+1
      silly_prefix <- as.integer(silly_prefix) #Not sure what the issue is here.
    }
  }
  silly_ids2[[i]] <- silly_list
}

  