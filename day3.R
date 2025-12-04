day3 <- readLines("day3.txt")

joltage <- rep(0, length(day3))
for(i in 1:length(day3)){
  teststr <- day3[[i]]
  for(j in 9:1){
    if(joltage[[i]]==0) {
    for(k in 9:1){
      if(!is.na(str_extract(teststr, sprintf("[%i](?:[0-%i]*)[%i]", j, j-1, k)))) {
        joltage[[i]] <- as.numeric(paste0(j, k))
        break
      }
    }
    } 
  }
}

#Part 2
#Steps: find the largest sequence of 12 digits. 
joltage2 <- vector()
for(i in 1:length(day3)) {
  teststr <- day3[[i]]
  substr <- str_sub(teststr,1, -12)
  match_found <- F
  j <- 9
  while(!match_found) {
    strpos <- str_locate_all(substr, as.character(j))
    if(!length(strpos[[1]])==0){
    #strpos <- strpos |> as.data.frame() |> select(start)
    substr2 <- str_sub(teststr, strpos[[1]][1,1])
    match_found <- T
    } else {
      j <- j-1
    }
  }
  
  k <- 9
  rounds <- 1
  while(k>0){
    if(rounds > 1){
      str_mini <- str_sub(substr2, rounds)
      vec <- sapply(1:nchar(str_mini), FUN=function(x){
        str_sub(str_mini, x, x)
      })
      if(all(vec==vec[[1]])){
        substr2 <- paste0(str_sub(substr2, 1, rounds), paste(rep(vec[[1]], 12-rounds), collapse=""))
        k <- 0
      }
    } 
    if(rounds==11 & nchar(substr2>12)){
      str_mini <- str_sub(substr2, 12)
      max_char <- 0
      for(m in 1:nchar(str_mini)){
        num <- as.numeric(str_sub(str_mini, m, m))
        if(num>max_char){
          max_char <- num
        }
      }
      substr2 <- paste0(str_sub(substr2, 1, 11), max_char)
      k <- 0
    } else {
  delstr <- str_extract(substr2, sprintf("%i+([^%i]+?)%i", j, k, k), group=1)
  if(!is.na(delstr)){
  substr3 <- str_remove(substr2, delstr)
  if(nchar(substr3)>=12){
    substr2 <- substr3
    j <- k
    k <- 9
    rounds <- rounds+1
  } else {
    k <- k-1
  }
  } else {
    k <- k-1
  }
    }
  }
  joltage2 <- c(joltage2, as.numeric(substr2))
}

#Part 2
str_to_vec <- function(string){
  vec <- vector()  
  for(i in 1:nchar(string)){
    vec <- c(vec, as.numeric(str_sub(string, i, i)))
  }
  return(vec)
}

day3fun <- function(day3) {
  joltage2 <- vector()
  for(i in 1:length(day3)) {
    vec <- str_to_vec(day3[[i]])
    global_max <- max(vec[1:(length(vec)-11)])
    vec <- vec[min(which(vec==global_max)):length(vec)]
    pos <- 2
    
    while(length(vec)>12) {
      num <- vec[[pos]]
      local_max <- max(vec[(pos+1):length(vec)])
      local_index <- min(which(vec[(pos+1):length(vec)]==local_max))+pos
      if(num >= local_max){
        if(pos>=12){
          vec <- vec[1:12]
        } else {
          pos <- pos + 1
        }
      } else {
        startpos <- pos
        while(pos==startpos){
          if(any(local_max==vec[(pos+1):length(vec)])){
            local_index <- min(which(vec[(pos+1):length(vec)]==local_max))+pos
            if((length(vec) - (local_index-pos)) > 11) {
              vec <- c(vec[1:pos-1], vec[local_index:length(vec)])
              pos <- pos+1
            }
          }
          if(pos==startpos){
            local_max <- local_max - 1
            if(local_max==num){
              if(pos==12) {
                vec <- vec[1:12]
              }
              pos <- pos+1
            }
          }
        }
        if(pos > 12){
          pos <- 12
        }
      }
    }
    outnum <- paste0(vec, collapse="") |> as.numeric()
    joltage2 <- c(joltage2, outnum)
  }
  return(joltage2)
}


