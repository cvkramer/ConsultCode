isOdd <- function(int){
  if(int/2==floor(int/2)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

findNewMax <- function(vec){
  max <- 0
  for(i in 1:length(vec)){
    if(vec[[i]] > max){print(paste(i,":",vec[[i]]));max <- vec[[i]]}
    
  }
}

printCollatz <- function(int,rep=0){
  if(int==8){return(rep)}
  if(isOdd(int)){
    new <- 3*int+1
  }else{
    new <- int/2
  }
  #print(int)
  printCollatz(new,rep+1)
}

reps <- 10000
x <- c(8:reps)
y <- as.vector(lapply(x,printCollatz))
plot(x,y)

findNewMax(y)


