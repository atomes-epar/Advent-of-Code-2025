library(sf)
library(gmp)
library(dplyr)
library(reshape2)
day9 <- read.csv("Advent of Code/day9.txt", header=F)
names(day9) <- c("x", "y")
day9 <- day9 |> arrange(x, y)

day9_distx <- dist(day9$x) |> as.matrix()
day9_disty <- dist(day9$y) |> as.matrix()
day9_distx[upper.tri(day9_distx, diag=T)] <- NA
day9_disty[upper.tri(day9_disty, diag=T)] <- NA
dists_dfy <- melt(day9_disty, na.rm=T, value.name="ydist")
dists_dfx <- melt(day9_distx, na.rm=T, value.name="xdist")

dists_df <- merge(dists_dfx, dists_dfy)
dists_df <- dists_df |> mutate(area=xdist*ydist)

var1 <- dists_df$Var1[which(dists_df$area==max(dists_df$area))]
var2 <- dists_df$Var2[which(dists_df$area==max(dists_df$area))]


x1 <- day9$x[var1]
x2 <- day9$x[var2]
y1 <- day9$y[var1]
y2 <- day9$y[var2]

#Ranges are inclusive :(
area=as.bigz(abs(x2-x1)+1)*as.bigz(abs(y2-y1)+1)

day9_poly <- read.csv("Advent of Code/day9.txt", header=F)
day9_poly[497,] <- day9_poly[1,]
day9_poly <- st_polygon(list(as.matrix(day9_poly)))
#plot(day9_poly)

dists_df <- dists_df |> arrange(desc(area))
solution <- F
i <- 1
#Not fast! The solution space could probably be filtered to remove anything that crosses the anti-cheat strip to save time.
while(!solution){
  var1 <- dists_df$Var1[i]
  var2 <- dists_df$Var2[i]
  x1 <- day9$x[var1]
  x2 <- day9$x[var2]
  y1 <- day9$y[var1]
  y2 <- day9$y[var2]
  rect <- matrix(c(min(x1,x2), min(y1,y2), max(x1,x2), min(y1,y2), max(x1,x2), max(y1,y2), min(x1,x2), max(y1,y2), min(x1,x2), min(y1,y2)), ncol=2, byrow=T)
  rect <- st_polygon(list(rect))
  solution <- st_contains(day9_poly, rect, sparse=F)[1,1]
  i <- i+1
}
area=as.bigz(abs(x2-x1)+1)*as.bigz(abs(y2-y1)+1)

plot(day9_poly)
plot(rect, border="red3", add=T)
