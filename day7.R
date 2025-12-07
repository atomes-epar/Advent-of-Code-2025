library(stringr)
library(asciicast)
library(crayon)

day7 <- readLines("day7.txt")

nlines <- length(day7)

day7 <- str_split(paste(day7, collapse=""), "") |> unlist()
day7 <- matrix(day7, nrow=nlines, byrow=T)

tree <- list()
tree <- c(tree, str_replace_all(day7[1,], ".", " ")) #Start the tree
splits <- 0
for(i in 2:nlines){
  beams <- c(which(day7[(i-1),]=="S"), which(day7[(i-1),]=="|"))
  splitters <- which(day7[i,]=="^")
  splitters <- splitters[which(splitters %in% beams)]
  splits <- splits+length(splitters)
  beams <- beams[which(!(beams %in% splitters))] #Don't replace the splitters with beams.
  replace_values <- c(beams, if(length(splitters>0)) splitters+1, splitters-1) |> unique()
  day7[i, replace_values] <- "|"
  tree <- c(tree, str_replace_all(day7[1:i,], ".", " ")) #For animation - this ends up taking up a lot of space.
}

splits

#Part2
num_day7 <- day7[1:3,]
num_day7[num_day7=="S" | num_day7=="^" | num_day7=="."] <- "0"
num_day7[num_day7=="|"] <- "1"
num_day7 <- matrix(as.numeric(num_day7), nrow=3)

day7fun <- function(){
for(i in 4:nlines){
  if((i %% 2)==0){
    num_day7 <- rbind(num_day7, num_day7[i-1,])
  } else {
    beams <- which(day7[i,]=="|")
    beamsums <- sapply(beams, function(x){
      beam_left <- max(beam-1, 1)
      beam_right <- min(beam+1, ncol(num_day7))
      new_beam <- num_day7[i-1,beam]+num_day7[i-1, beam_left]*(day7[i,beam_left]=="^") + num_day7[i-1, beam_right] * (day7[i,beam_right]=="^")
      return(new_beam)
    })
    new_row <- rep(0, ncol(num_day7))
    new_row[beams] <- beamsums
    num_day7 <- rbind(num_day7, new_row)
  }
}
}
sum(num_day7[nrow(num_day7),])
  