#Find maximum value using loop and condition only

x <- c(5, 12, 3, 20, 8)
max_val <- x[1]
for (i in x) {
  if (i> max_val){
    max_val <- i
  }
}
max_val



