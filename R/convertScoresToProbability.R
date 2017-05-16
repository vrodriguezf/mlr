library(trustOptim)
library(e1071)

convertingScoresToProbability = function(anomaly.score, parainit, method = "mixture-model"){
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
      t = ifelse(p[1] * f + p[2] > 0, 0, 1)
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
  else if (method == "mixture-model" ) {
    if (is.null(p)) {
      p = c(mu = 1, sigma = 1, lambda = 1, alpha = 0.5)
      messagef("Starting parameter values are missing. For calibration using mixture modeling the default is: \n mu = 1, sigma = 1, lambda = 1, alpha = 1")
    }
    if (length(p) != 4) {
      stop("Too little/many starting parameters. For calibration using sigmoid function starting values for parameter mu, sigma, lambda, alpha are needed.")
    }

    #p1 = mu, p2 = sigma, p3=lambda, p4 = alpha
    gaussian = function(p) { 1 / (sqrt(2 * pi) * p[4]) * exp(-(f-p[1])^2 / (2 * p[2]^2)) }
    exponential = function(p) { p[3] * exp (-p[3] * f) }
    posteriori = function(p) { (p[4] * gaussian(p)) / (p[4] * gaussian(p) + (1-p[4]) * exponential(p)) }

    while (loop) {
      t = posteriori(p)
      sumt = rep(1,length(t)) %*% t
      tf = t(t) %*% f
      mu = tf / sumt
      sigma = (t(t) %*% (f - as.numeric(mu))^2) / sumt
      lambda = sumt / tf
      alpha = sumt / length(t)
      pnew = c(mu, sigma, lambda, alpha)

      if(any(is.na(pnew))) {
        diff = FALSE
        pnew = p
      } else {
        diff = sum(abs(pnew - p)) > 1e-3
      }
      if ( diff ) {
        loop = TRUE
        p = pnew
      } else {
        loop = FALSE
        list$optim.solution = pnew
        list$probability = posteriori(pnew)
      }
    }
  } else {
    if (is.null(p)) {
      p = c(mu = 1, sigma = 1, lambda = 1, alpha = 0.5)
      messagef("Starting parameter values are missing. For calibration using mixture modeling the default is: \n mu = 1, sigma = 1, lambda = 1, alpha = 1")
    }
    if (length(p) != 4) {
      stop("Too little/many starting parameters. For calibration using sigmoid function starting values for parameter mu, sigma, lambda, alpha are needed.")
    }

    #p1 = mu, p2 = sigma, p3=lambda, p4 = alpha
    gaussian = function(p) { 1 / (sqrt(2 * pi) * p[4]) * exp(-(f-p[1])^2 / (2 * p[2]^2)) }
    exponential = function(p) { p[3] * exp (-p[3] * f) }
    posteriori = function(p) { (p[4] * gaussian(p)) / (p[4] * gaussian(p) + (1-p[4]) * exponential(p)) }

    while (loop) {
      t = posteriori(p)
      LL = function(p) { -sum(t * log(p[4] * gaussian(p)) + (1-t) * log((1-p[4]) * exponential(p))) }
      gmu = function(p) { sum(t * ((f - p[1])^2 + 1) / p[2]^3) }
      gsigma = function(p) { -sum(t * (f - p[1])^2 / p[2]^2) }
      glambda = function(p) { -sum((1-t) * (1/p[3] - f)) }
      galpha = function(p) { -sum( (t-p[4]) / (p[4]*(1-p[4]))) }
      g = function(o) {
        c(gmu(p), gsigma(p), glambda(p), galpha(p))
      }
      optim = trust.optim(p, fn = LL, gr = g,  method = "BFGS", control = list(report.level = 0))
      pnew = optim$solution
      diff = sum(abs(pnew - p))

      if ( diff > 1e-2) {
        loop = TRUE
        p = pnew
      } else {
        loop = FALSE
        list$optim.solution = pnew
        list$probability = 1 / (1 + exp(-pnew[1] * f - pnew[2]))
      }
    }
  }
  return(list)
}


#### sanity check - sigmoid
data = readRDS("/Users/Minh/Documents/TestData/data_anomaly.rds")
svm.model <- svm(data[,1:4], y = NULL, type='one-classification', kernel="radial", nu =0.05)
svm.pred <- predict(svm.model, data[,1:4])
table(data[,5]) # TRUE  = normal (hat keine Einfluss auf das Model)
table(svm.pred) # SVM setzt automatisch TRUE = normal

# decision values
dv = svm.model$decision.values
plot(1:1050, dv)
o1 = order(dv) #Index der sortierten decision values von klein nach groß
head(dv[o1])
#Die ersten 50 beobachtungen sind Anomalien, prüfe ob die niedrigsten 50 decision values zu den anomalen Beobachtungen gehören
setdiff(1:50, o1[1:50]) # bis auf drei anomalien, gehören die restlichen anomalien zu den niedrigsten decision values.
#-> es scheint kleine values sprechen für Anomalien


# check: converting to probability
p = c(1,0) # initial parameter (vorschlag aus paper, finde paper wieder)
# NOTE Sigmoid: nach paper http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.448.3178&rep=rep1&type=pdf
# gilt t= 1 = Anomaly unt t=0=Normal (formel (9))
# Nach dem implementierne habe ich gemerkt, dass kleine Wahrscheinlichkeiten für eine Anomaly sprechen
# im Sinne von "Die Wskeit für "normal" ist klein, daher ist die Beobachtung eher nicht normal (=anomaly)"
# Deckt sich mit den decision values vom SVM; kleine decision boundary -> eher anomaly
# Wenn man eine umgekehrte Interpretation (kleine prob -> keine anomaly,große prop -> anomaly)
# will dann muss man t = 0 = anomaly und t = 1 = normal in Formel (9) setzen (das habe ich hier gemacht))
prop = convertingScoresToProbability(dv, parainit = p, method = "sigmoid")
plot(1:1050, prop$probability, ylim = c(0,1)) # ersten 50 Beobachtungen sind anomalien -> kleine prop -> wskeit für normal ist gering ->
o = rev(order(prop$probability)) #große prob zuerst
head(prop$probability[o], 50)
setdiff(1:50, o[1:50]) #in data sind die ersten 50 Beobachtungen anomalien, daher sollten die auch die höchsten Probabilities haben


#### sanity check - mixture-model
p = c(0.5,0.5,0.5,0.5)
p = c(1,0.5,1,0.5)
prop = convertingScoresToProbability(dv, p, method = "mixture-model2")
plot(1:1050, prop$probability, ylim = c(0,1))
length(which(prop$probability == 0))
o = order(prop$probability)
head(prop$probability)
head(prop$probability[o])
#https://www.researchgate.net/profile/John_Platt/publication/2594015_Probabilistic_Outputs_for_Support_Vector_Machines_and_Comparisons_to_Regularized_Likelihood_Methods/links/004635154cff5262d6000000.pdf
setdiff(1:50, o[1:50])
