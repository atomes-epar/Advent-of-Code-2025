day5 <- readLines("day5.txt")

fresh_ranges <- day5[which(str_detect(day5, "-"))]
products <- day5[which(!str_detect(day5, "-"))]

freshdf <- do.call(rbind, lapply(fresh_ranges, FUN=function(x){
  x <- str_split(x, "-") |> unlist() |> as.bigz()
  data.frame(min=x[[1]], max=x[[2]])
}))

freshdf <- freshdf[order(freshdf$min),]



products <- products[nzchar(products)] |> as.bigz()

search_products <- function(prod_list, ranges){
  output <- sapply(prod_list, FUN=function(prod){
    min_min <- which(ranges$min <= prod)
    max_max <- which(ranges$max >= prod)
    if(any(min_min %in% max_max)) return(1) else return(0)
  })
  return(output)
}

#debugonce(search_products)
#num_fresh <- search_products(as.bigz("1282950124751"), freshdf)
num_fresh <- search_products(products, freshdf)
sum(num_fresh)

#part2 
#combine overlapping ranges
newdf <- freshdf
df_rows <- nrow(freshdf)
stop <- F
while(stop==F){
  i <- 1
  while(i <= nrow(newdf)) {
    testmin <- newdf$min[i]
    testmax <- newdf$max[i]
    #There's probably an easier way to do this.
    rows <- which((newdf$min==testmin | newdf$min==testmax | newdf$max==testmin | newdf$max==testmax) |
                    (newdf$min > testmin & newdf$min < testmax) | 
                    (newdf$min < testmin & newdf$min < testmax & newdf$max > testmin))
    if(length(rows) > 1) {
      subdf <- data.frame(min=min(newdf$min[rows]), max=max(newdf$max[rows]))
      if(exists("outdf")){
        outdf <- rbind(outdf, subdf)
      } else {
        outdf <- subdf
      }
      newdf <- newdf[-rows,]
    } else {
      if(exists("outdf")){
        outdf <- rbind(outdf, newdf[i,])
      } else {
        outdf <- newdf[i,]
      }
      i <- i+1
    }
  }
  if(nrow(outdf) < df_rows){
    newdf <- outdf
    rm(outdf)
    df_rows <- nrow(newdf)
  } else {
    stop <- T
  }
}

outdf <- outdf[order(outdf$min),]
#verify
num_fresh <- search_products(products, outdf)
sum(num_fresh)

outdf$diff <- (outdf$max-outdf$min)+1 #ranges are inclusive
sum(outdf$diff)