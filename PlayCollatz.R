isOdd <- function(int){
  if(int/2==floor(int/2)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

printCollatz <- function(int,prev=c(0,0,0),rep=0){
  end <- c(4,2,1)
  if(sum(prev==end)==3){return("done")}
  if(isOdd(int)){
    new <- 3*int+1
  }else{
    new <- int/2
  }
  print(int)
  prev <- c(prev[2],prev[3],new)
  printCollatz(new,prev,rep+1)
}