#' @title Convert anomaly scores to probability estimates.
#'
#' @description Convert anomaly scores to probability estimates. The higher the probability estimate the more likely the observation belongs to the normal class.
#'
#' @param anomaly.score a numeric vector of anomaly scores.
#' @param parainit a vector of starting values for the optimizer
#' @param method  valid argument are \code{sigmoid} or \code{mixture-model}
#' @return [\code{vector}] with probabilities as entries.
#' @export
#' @references Gao, Jing, and Pang-Ning Tan. "Converting output scores from outlier detection algorithms into probability estimates." Data Mining, 2006. ICDM'06. Sixth International Conference on. IEEE, 2006.
#' @examples
#'
#' # Data = data[, 1:4] # find better dataset later
#' svm.model <- svm(Data, y = NULL, type = 'one-classification', kernel = "radial", nu = 0.05)
#  svm.pred <- predict(svm.model, Data)
#' dv = svm.model$decision.values
#' prop = convertingScoresToProbability(dv, parainit = c(0, 1), method = "sigmoid")
#' plot(1:length(prop$probability), prop$probability, ylim = c(0, 1))
#'
convertingScoresToProbability = function(anomaly.score, parainit, method = "sigmoid"){
  match(method, c("sigmoid", "mixture-model", "mixture-model2"))
  f = anomaly.score
  p = parainit
  loop = TRUE
  sum.help = rep(1, length(f))
  list = list()

  if( method == "sigmoid") {
    if (is.null(p)) {
      p = c(A = 1, B = 0)
      messagef("Starting parameter values are missing. For calibration using sigmoid function the default is: \n A = 1, B = 0")
    }
    if (length(p) != 2) {
      stop("Too little/many starting parameters. For calibration using sigmoid function starting values for parameter A and B are needed.")
    }

    while (loop) {
      t = ifelse(p[1] * f + p[2] > 0, 1, 0)
      # LL and its derivatives
      LL = function(p) { t((1-t)) %*% (p[1] * f + p[2]) + sum.help %*% log(1 + exp(-p[1] * f - p[2])) }
      gA = function(p) { t(f) %*% ( (1-t) - (1 / exp(p[1] * f + p[2]))) }
      gB = function(p) { sum.help %*% ((1-t) - (1 / exp(p[1]*f + p[2]))) }
      g = function(p) {
        c(gA(p), gB(p))
      }
      hAA = function(p) { t(f^2) %*% (exp( p[1]*f + p[2]) / (1 + exp(p[1]*f + p[2]))^2) }
      hBB = function(p) { sum.help %*% (exp(p[1] * f + p[2]) / (1 + exp(p[1] * f + p[2]))^2) }
      hAB = function(p) { t(f) %*% (exp( p[1]*f + p[2]) / (1 + exp(p[1]*f + p[2]))^2) }
      h = function(p) {
        as(matrix(c(hAA(p), hAB(p),hAB(p), hBB(p)), 2, 2), "dgCMatrix")
      }

      optim = trust.optim(p, fn = LL, gr = g,  method = "BFGS", control = list(report.level = 0))
      pnew = optim$solution
      diff = sum(abs(pnew - p))

      if ( diff > 1e-4) {
        loop = TRUE
        p = pnew
      } else {
        loop = FALSE
        list$optim.solution = pnew
        list$probability = 1 / (1 + exp(-pnew[1] * f - pnew[2]))
      }
    }
  }
  # else if (method == "mixture-model" ) {
  #   if (is.null(p)) {
  #     p = c(mu = 1, sigma = 1, lambda = 1, alpha = 0.5)
  #     messagef("Starting parameter values are missing. For calibration using mixture modeling the default is: \n mu = 1, sigma = 1, lambda = 1, alpha = 1")
  #   }
  #   if (length(p) != 4) {
  #     stop("Too little/many starting parameters. For calibration using sigmoid function starting values for parameter mu, sigma, lambda, alpha are needed.")
  #   }
  #
  #   #p1 = mu, p2 = sigma, p3=lambda, p4 = alpha
  #   gaussian = function(p) { 1 / (sqrt(2 * pi) * p[4]) * exp(-(f-p[1])^2 / (2 * p[2]^2)) }
  #   exponential = function(p) { p[3] * exp (-p[3] * f) }
  #   posteriori = function(p) { (p[4] * gaussian(p)) / (p[4] * gaussian(p) + (1-p[4]) * exponential(p)) }
  #
  #   while (loop) {
  #     t = posteriori(p)
  #     sumt = rep(1,length(t)) %*% t
  #     tf = t(t) %*% f
  #     mu = tf / sumt
  #     sigma = (t(t) %*% (f - as.numeric(mu))^2) / sumt
  #     lambda = sumt / tf
  #     alpha = sumt / length(t)
  #     pnew = c(mu, sigma, lambda, alpha)
  #
  #     if(any(is.na(pnew))) {
  #       diff = FALSE
  #       pnew = p
  #     } else {
  #       diff = sum(abs(pnew - p)) > 1e-3
  #     }
  #     if ( diff ) {
  #       loop = TRUE
  #       p = pnew
  #     } else {
  #       loop = FALSE
  #       list$optim.solution = pnew
  #       list$probability = posteriori(pnew)
  #     }
  #   }
  # } else {
  #   if (is.null(p)) {
  #     p = c(mu = 1, sigma = 1, lambda = 1, alpha = 0.5)
  #     messagef("Starting parameter values are missing. For calibration using mixture modeling the default is: \n mu = 1, sigma = 1, lambda = 1, alpha = 1")
  #   }
  #   if (length(p) != 4) {
  #     stop("Too little/many starting parameters. For calibration using sigmoid function starting values for parameter mu, sigma, lambda, alpha are needed.")
  #   }
  #
  #   #p1 = mu, p2 = sigma, p3=lambda, p4 = alpha
  #   gaussian = function(p) { 1 / (sqrt(2 * pi) * p[4]) * exp(-(f-p[1])^2 / (2 * p[2]^2)) }
  #   exponential = function(p) { p[3] * exp (-p[3] * f) }
  #   posteriori = function(p) { (p[4] * gaussian(p)) / (p[4] * gaussian(p) + (1-p[4]) * exponential(p)) }
  #
  #   while (loop) {
  #     t = posteriori(p)
  #     LL = function(p) { -sum(t * log(p[4] * gaussian(p)) + (1-t) * log((1-p[4]) * exponential(p))) }
  #     gmu = function(p) { sum(t * ((f - p[1])^2 + 1) / p[2]^3) }
  #     gsigma = function(p) { -sum(t * (f - p[1])^2 / p[2]^2) }
  #     glambda = function(p) { -sum((1-t) * (1/p[3] - f)) }
  #     galpha = function(p) { -sum( (t-p[4]) / (p[4]*(1-p[4]))) }
  #     g = function(o) {
  #       c(gmu(p), gsigma(p), glambda(p), galpha(p))
  #     }
  #     optim = trust.optim(p, fn = LL, gr = g,  method = "BFGS", control = list(report.level = 0))
  #     pnew = optim$solution
  #     diff = sum(abs(pnew - p))
  #
  #     if ( diff > 1e-2) {
  #       loop = TRUE
  #       p = pnew
  #     } else {
  #       loop = FALSE
  #       list$optim.solution = pnew
  #       list$probability = 1 / (1 + exp(-pnew[1] * f - pnew[2]))
  #     }
  #   }
  # }
  return(list)
}

