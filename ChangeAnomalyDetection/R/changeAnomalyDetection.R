changeAnomalyDetection <-
function(x, term=30, smooth.n=7, order=c(1,0,0), ...){
  N <- length(x)
  outlier.score <- sapply(1:(N-term-1), function(i){
    train <- x[i:(i+term)]
    target <- x[(i+term+1)]
    fit <- arima(train, order=order,...)
    pred <- as.numeric(predict(fit, n.ahead=1)$pred)
    outlierScore(train=fit$residuals, x=(pred - target))
  })

  outlier.score.smooth <- na.omit(scoreSmoothing(outlier.score, smooth.n))

  N2 <- length(outlier.score.smooth)
  change.score <- sapply(1:(N2-term-1), function(i){
    train <- outlier.score.smooth[i:(i+term)]
    target <- outlier.score.smooth[(i+term+1)]
    outlierScore(train=train, x=target)
  })
  change.score.smooth <- na.omit(scoreSmoothing(change.score, round(smooth.n/2)))
  c(rep(0,(N-length(change.score.smooth))), change.score.smooth)
}
