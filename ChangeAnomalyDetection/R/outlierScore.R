outlierScore <-
function(train, x, ...){
  m <- mean(train)
  s <- sd(train)
  -log(dnorm(x, m, s))
}
