distfun <- function(p, q){
  res <- (q$x-p$x)^2+(q$y-p$y)^2+(q$z-p$z)^2
  return(res)
}

distances_out <- data.frame(id=numeric(), dest=numeric(), dist=numeric())

for(i in 1:(nrow(day8)-1)){
  distances <- sapply((i+1):nrow(day8), function(x) distfun(day8[i,], day8[x,]))
  distances <- data.frame(id=rep(i, length(distances)), dest=seq(i+1, nrow(day8)), dist=distances)
  distances_out <- bind_rows(distances_out, distances)
}
distances_out <- arrange(distances_out, dist)
day8_connections <- distances_out[1:1000,]


check_nodes <- function(circuits, connection){
  which(sapply(circuits, function(circuit) any(circuit %in% connection))!=0)
}

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