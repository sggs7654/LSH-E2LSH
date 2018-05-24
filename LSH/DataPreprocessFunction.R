Dist <- function(x,i,j){
  px <- x[which(x$index == i),]
  py <- x[which(x$index == j),]
  ix <- px[[2]]
  iy <- px[[3]]
  jx <- py[[2]]
  jy <- py[[3]]
  result <- sqrt((ix - jx)^2 + (iy - jy)^2)
  result
}

check <- function(x,querySet,px,py){
  result <- TRUE
  for(i in querySet){
    q <- x[which(x$index == i),]
    qx <- q[[2]]
    qy <- q[[3]]
    d <- sqrt((qx - px)^2 + (qy - py)^2)
    if(d <= q[[5]]){
      result <- FALSE
    }
  }
  result
}