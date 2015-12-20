isOdd <- function(int){
  if(int/2==floor(int/2)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

printCollatz <- function(int,end,rep=0){
  if(rep==end){stop('Completed Function. \n\n')}
  if(isOdd(int)){
    new <- 3*int+1
  }else{
    new <- int/2
  }
  print(int)
  printCollatz(new,end,rep+1)
}