library(stringr)
df <- readLines("day1.txt")
df <- data.frame(raw=df)

df$num <- as.numeric(str_extract(df$raw, "[0-9]+")) * (-1*str_detect(df$raw, "L")+str_detect(df$raw, "R"))
df$dialpos <- 50
for(i in 1:nrow(df)){
  currpos <- if(i==1) 50 else df$dialpos[[i-1]]
  turndist <- df$num[[i]] %% 100
  newpos <- currpos + turndist 
  if(newpos < 0) newpos <- newpos+100
  if(newpos > 99) newpos <- newpos-100
  df$dialpos[[i]] <- newpos
}

print(sprintf("The password is %i", sum(df$dialpos==0)))

#Method 0x434C49434B
df$dialpos2 <- 0
df$numzeros <- 0
for(i in 1:nrow(df)){
  currpos <- if(i==1) 50 else df$dialpos[[i-1]]
  num <- df$num[[i]]
  turndist <- (abs(num) %% 100)*abs(num)/num
  cycles <- abs(num) %/% 100
  newpos <- currpos + turndist 
  if(newpos < 0) {
    newpos <- newpos+100
    if(currpos!=0) cycles <- cycles+1
  } else if(newpos > 99) {
    newpos <- newpos-100
    cycles <- cycles+1
  } else if(newpos==0) {
    cycles <- cycles+1
  }
  df$dialpos2[[i]] <- newpos
  df$numzeros[[i]] <- cycles
}

print(sprintf("The password is %i", sum(df$numzeros)))