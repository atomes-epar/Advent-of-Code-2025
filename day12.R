library(stringr)
day12 <- readLines("day12.txt")

gifts <- which(!is.na(str_locate(day12, "^[0-9]:")))

gifts <- gifts[gifts<1000] #kludge - not sure what's getting picked up here.
#Okay I peeked at reddit. Today's lesson: always eliminate the trivial cases first. 

gift_matrices <- lapply(gifts, function(gift){
  gift_outline <- day12[(gift+1):(gift+3)]
  gift_outline <- str_replace_all(gift_outline, "#", "1")
  gift_outline <- str_replace_all(gift_outline, "\\.","0")
  gift_outline <- str_split(paste0(gift_outline, collapse=""), "") |> unlist() |> as.numeric()
  gift_outline <- matrix(gift_outline, nrow=3, byrow=T) 
})

check_gifts <- function(gift_matrices, grid, n_gifts){
  giftsums <- sapply(gift_matrices, sum)
  totgifts <- sum(n_gifts)
  tot_squares <- sum(giftsums*n_gifts)
  grid_area <- grid[[1]]*grid[[2]]
  if(grid_area >= totgifts*9) {
    return("Match")
  } else if(tot_squares > grid_area) {
    return("No Match")
  } else {
    return("Maybe match")
  }
}

tots <- sapply(gift_matrices, sum)

present_grids <- lapply(day12, function(x) str_extract_all(x, "([0-9]{2})x([0-9]{2})")) |> unlist()
present_grids <- str_split(present_grids, "x") |> lapply(as.numeric) #|> lapply(function(x) Reduce(`*`, x))
day12_2 <- day12[which(str_detect(day12, "x"))]
day12_2 <- str_extract(day12_2, ": (.+)", group=1)
day12_2 <- lapply(day12_2, function(x) str_split(x, " ") |> unlist() |> as.numeric()) # |> sum())


result <- lapply(1:length(day12_2), function(x) check_gifts(gift_matrices, present_grids[[x]], day12_2[[x]]))
table(unlist(result))

