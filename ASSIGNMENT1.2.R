#Replace NA values with mean of non-NA elements(without mean())

x <- c(10, NA, 20, NA, 30)

sum <- 0
count <- 0

for (i in x){
  if(!is.na(i)){
  sum <- sum+i
  count <- count+1
  }
}
mean_val <- sum/count

for (i in 1:length(x)){
  if (is.na(x[i])){
    x[i] <- mean_val
  }
}
x