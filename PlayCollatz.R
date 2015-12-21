isOdd <- function(int){
  if(int/2==floor(int/2)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

findNewMax <- function(vec){
  max <- 0
  ind <- c()
  res  <- c()
  for(i in 1:length(vec)){
    if(vec[[i]] > max){
      ind <- c(ind,i); 
      res <- c(res,vec[[i]]);
      max <- vec[[i]]
      }
  }
  return(as.data.frame(cbind(ind,res))) #list[i=ind,max=res]
}

printCollatz <- function(int,rep=0,isodd=0,iseven=0){
  if(int==8){return(c(rep,isodd,iseven))}#return(list(iter=rep,oddstep=isodd,evenstep=iseven))
  if(isOdd(int)){
    new <- 3*int+1
    isodd = isodd+1
  }else{
    new <- int/2
    iseven = iseven+1
  }
  #print(int)
  printCollatz(new,rep+1,isodd,iseven)
}

reps <- 1000
x <- c(8:reps)
output <- as.data.frame(NA,nrow=I(reps-8),ncol=4)
for(i in 8:reps){
  out <- printCollatz(i);
  output$i[i-7,1] <- i ;
  output$rep[i-7,2] <-  out[1];
  output$odd[i-7,3] <- out[2];
  output$even[i-7,4] <- out[3];
}

lapply(x,printCollatz)[[2]]

a <- findNewMax(y)
a$ind2 = a$ind^2


lm.a <- lm(a$res ~ a$ind + a$ind2)

plot(x,y)
points(a$ind, a$res, col='red', pch=20)
identify(x,y)
