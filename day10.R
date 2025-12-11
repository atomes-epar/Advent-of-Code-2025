library(stringr)
library(lpSolve)
library(dplyr)
day10 <- readLines("Advent of Code/day10.txt")

targets <- lapply(day10, function(str){
  start <- str_locate(str, "\\[")[[1]]
  end <- str_locate(str, "\\]")[[1]]
  target <- str_sub(str, start+1, end-1)
  target <- str_replace_all(target, "\\.", "0")
  target <- str_replace_all(target, "#", "1")
  as.matrix(t(unlist(str_split(target, ""))), nrow=1) |> as.numeric()
})

targets2 <- lapply(day10, function(str){
  start <- str_locate(str, "\\{")[[1]]
  end <- str_locate(str, "\\}")[[1]]
  str_split(str_sub(str, start+1, end-1), ",") |> unlist() |> as.vector() |> as.numeric()
})


button_banks <- lapply(1:length(day10), function(i){
  str <- day10[[i]]
  starts <- as.vector(str_locate_all(str, "\\(")[[1]][,1])
  ends <- as.vector(str_locate_all(str, "\\)")[[1]][,1])
  lights <- which(targets[[1]]==1)
  buttons <- lapply(1:length(starts), function(x){
    button <- str_split(str_sub(str, starts[[x]]+1, ends[[x]]-1), ",") |> unlist() |> as.vector() |> as.numeric()
    button <- button+1 #R is 1-indexed
    #Discard any buttons that don't toggle the lights we need on.
    #if(any(button %in% lights)) return(button) else return(NA)
  })
  buttons[!is.na(buttons)]
})

press_button <- function(lights, button){
  lights[button] <- lights[button]+1
  lights[lights==2] <- 0 #Toggle
  lights
}

#Tree search
#button_presses <- sapply(1:length(button_banks), function(i){
n_buttons <- vector()
for(i in 1:length(button_banks)){
  buttons <- button_banks[[i]]
  target <- targets[[i]]
  lights_start <- rep(0, length(target))
  button_presses <- 1
  button_nums <- lapply(buttons, function(button) press_button(lights_start, button))
  new_lights <- button_nums
  target_found <- any(sapply(new_lights, function(x) identical(x, target)))
  while(!target_found){
  button_presses <- button_presses+1
  new_lights <- lapply(new_lights, function(light) {
    lapply(button_nums, function(button){
      (light+button) %% 2
    })
  }) |> unlist(recursive=F) |> unique()
  target_found <- any(sapply(new_lights, function(x) identical(x, target)))
  }
  n_buttons <- c(n_buttons, button_presses)
}

n_buttons2 <- vector()
for(i in 1:length(button_banks)){
  buttons <- button_banks[[i]]
  target <- targets2[[i]]
  lights_start <- rep(0, length(target))
  #button_presses <- 1
  button_nums <- lapply(buttons, function(button) press_button(lights_start, button))
  mat <- matrix(unlist(button_nums), nrow=length(button_nums), ncol=length(target), byrow=T)
  result <- lp("min", rep(1, length(buttons)), t(mat), rep("==", length(target)), target, int.vec=seq(1,length(buttons)))
  n_buttons2 <- c(n_buttons2, sum(result$solution))
}

