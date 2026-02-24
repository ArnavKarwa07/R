x<- c(2,3,2,5,3,2)

visited <- rep(FALSE, length(x))

for (i in 1:length(x)){
  if(!visited[i]){
    count<-1
    for(j in (i+1):length(x)){
      if (x[i]== x[j]){
        count <- count+1
        visited[j]<-TRUE
      }
    }
    cat(x[i], ":", count, "\n")
  }
}