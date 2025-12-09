library(dplyr)
library(gmp)
library(reshape2)

day8 <- read.table("Advent of Code/day8.txt", sep=",")
names(day8) <- c("x","y","z")
day8 <- day8 |> arrange(x)

distfun <- function(p, q){
  res <- (q$x-p$x)^2+(q$y-p$y)^2+(q$z-p$z)^2
  return(res)
}

distances_out <- data.frame(id=numeric(), dest=numeric(), dist=numeric())
#Original solution
for(i in 1:(nrow(day8)-1)){
  distances <- sapply((i+1):nrow(day8), function(x) distfun(day8[i,], day8[x,]))
  distances <- data.frame(id=rep(i, length(distances)), dest=seq(i+1, nrow(day8)), dist=distances)
  distances_out <- bind_rows(distances_out, distances)
}
distances_out <- arrange(distances_out, dist)
day8_connections <- distances_out[1:1000,]

#Faster method - Man, I need to learn matrices.
df <- read.csv("Advent of Code/day8.txt", header = FALSE, col.names = c("x", "y", "z"))
pos <- as.matrix(df)
n_boxes <- nrow(pos)
dists <- dist(pos) |> as.matrix()
dists[upper.tri(dists, diag=T)] <- NA
dists_df <- melt(dists, na.rm=T)

distances_out <- arrange(dists_df, value)
day8_connections <- distances_out[1:1000,]
names(day8_connections) <- c("id", "dest", "dist")

check_nodes <- function(circuits, connection){
  which(sapply(circuits, function(circuit) any(circuit %in% connection))!=0)
}

#Original solution
circuits <- list()
for(i in 1:nrow(day8_connections)){
  connection <- c(day8_connections$id[i], day8_connections$dest[i])
  location <- check_nodes(circuits, connection)
  if(length(location) == 1){
    circuits[[location]] <- union(circuits[[location]], connection)
  } else if(length(location) > 1) {
    circuits[[min(location)]] <- union(unlist(circuits[location]), connection)
    circuits <- circuits[-location[location!=min(location)]]
  } else {
    circuits[[length(circuits)+1]] <- connection
  }
}
circuit_lengths <- sapply(circuits, length) 
circuit_lengths <- circuit_lengths[order(circuit_lengths, decreasing=T)]
Reduce(`*`, circuit_lengths[1:3])

#Slightly faster solution inspired by reddit
connections <- lapply(1:nrow(day8_connections), function(row) unlist(day8_connections[1,c("id","dest")], use.names=F))
check_nodes <- function(index, circuits, connection){
  which(sapply(circuits[(index+1):length(circuits)], function(circuit) any(circuit %in% connection))!=0)+index
}
i <- 1
while(i < length(connections)){
  con <- connections[i]
  location <- check_nodes(i, connections, con)
  if(length(location) > 1) {
    circuits[[min(location)]] <- union(circuits[location], con)
    circuits <- circuits[-location[location!=min(location)]]
  }
  i <- i+1
}
circuit_lengths2 <- sapply(connections, length) 
circuit_lengths2 <- circuit_lengths2[order(circuit_lengths2, decreasing=T)]
Reduce(`*`, circuit_lengths[1:3])

#part 2
circuits <- list()
is.done <- F
i <- 1
while(!is.done){
  connection <- c(distances_out$id[i], distances_out$dest[i])
  location <- check_nodes(circuits, connection)
  if(length(location) == 1){
    circuits[[location]] <- union(circuits[[location]], connection)
  } else if(length(location) > 1) {
    circuits[[min(location)]] <- union(unlist(circuits[location]), connection)
    circuits <- circuits[-location[location!=min(location)]]
  } else {
    circuits[[length(circuits)+1]] <- connection
  }
  if(length(circuits)==1 & length(circuits[[1]])==1000){
    x1 <- day8$x[day8$id==connection[1]]
    x2 <- day8$x[day8$id==connection[2]]
    as.bigz(x1)*as.bigz(x2)
    is.done <- T
  } else {
    i <- i + 1
  }
}
