outlierScore <-
function(train, x, ...){
  m <- mean(train)
  s <- sd(train)
  abs(-log(dnorm(x, m, s)))
}
