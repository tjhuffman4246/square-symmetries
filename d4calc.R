#d4calc.R - Symmetries of the square

D4.makeDataFrame <- function() {
  DF <- data.frame(name=rep("",8),cfg=rep("",8),stringsAsFactors = FALSE)
  DF[1,] <- c("i","ABCD")
  DF[2,] <- c("r1","DABC")
  DF[3,] <- c("r2","CDAB")
  DF[4,] <- c("r3","BCDA")
  DF[5,] <- c("m1","BADC")
  DF[6,] <- c("m2","DCBA")
  DF[7,] <- c("d1", "ADCB")
  DF[8,] <- c("d2", "CBAD")
  return(DF)
}

DF <- D4.makeDataFrame()
D4.showConfigs <- function(DF) {
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,24),ylim = c(-1,3), asp = 1, axes = FALSE)
  for (i in 0:7) {
    points(c(0,2,2,0,0)+3*i,c(0,0,2,2,0),type = "l")
    lbl <- strsplit(DF[i+1,2],"")[[1]]
    text(c(0.25,1.75,1.75,0.25)+3*i,c(1.75,1.75,0.25,0.25),lbl)
    text(1+3*i,-0.5,DF[i+1,1])
    segments(c(13,15,18,21),c(0,1,2,0),
             c(13,17,20,23),
             c(2,1,0,2),lty = 2)
  }
}
D4.showConfigs(DF)

#cfg is a string of symbols, reading counterclockwise from the top
D4.showSquare <- function(cfg){
  par(mar=c(1,1,1,1))
  plot(NULL,xlim=c(0,3),ylim = c(-1,2), asp = 1, axes = FALSE)
  points(c(0,2,2,0,0),c(0,0,2,2,0),type = "l", lwd = 2)
  lbl <- strsplit(cfg,"")[[1]]
  text(c(0.15,1.85,1.85,0.15),c(1.85,1.85,0.15,0.15),lbl)
}
D4.showSquare("ABCD")

#a is one of the Biggs symbols for an operation

#The return value is the new configuration
D4.apply <- function(a,cfg){
  v <-strsplit(cfg,"")[[1]]
  w <- switch(a,
              "i" = v,
              "r1" = c(v[4],v[1],v[2],v[3]),
              "r2" = c(v[3],v[4],v[1],v[2]),
              "r3" = c(v[2],v[3],v[4],v[1]),
              "m1" = c(v[2],v[1],v[4],v[3]),
              "m2" = c(v[4],v[3],v[2],v[1]),
              "d1" = c(v[1],v[4],v[3],v[2]),
              "d2" = c(v[3],v[2],v[1],v[4])
  )
  s <- paste(w,sep="",collapse="") 
  return(s)
}


D4.multiply <- function(DF,a,b){
  #Look up the name
  idx <- which(DF$name==b)[1]
  #Find the corresponding configuration
  cfg <- DF$cfg[idx]
  #Apply the group operation to it
  newcfg <- D4.apply(a,cfg)
 # Look up the configuration
  idx <- which(DF$cfg==newcfg)[1]
  return (DF$name[idx])
}
vD4.multiply <- Vectorize(D4.multiply,c("a","b"))

