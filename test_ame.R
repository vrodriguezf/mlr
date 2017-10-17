library(data.table)
library(BBmisc)
library(devtools)
library(margins)
# see also https://diffuseprior.wordpress.com/2012/04/23/probitlogit-marginal-effects-in-r-2/

load_all()
mtcars$cyl = as.factor(mtcars$cyl)
mt.task = makeRegrTask(data = mtcars, target = "mpg")
lrn = makeLearner("regr.lm")

################################################################################
# use case 1: in a lm without interactions, the ame should equate to the coefficients!
################################################################################
mt.task.small = subsetTask(mt.task, features = c("cyl", "hp", "carb"))
m = train(lrn, mt.task.small)
mod = getLearnerModel(m)

# compare with margins github package https://github.com/leeper/margins
ame = summary(margins(mod, data = mtcars))$AME
#colMeans(marginal_effects(mod, data = mtcars, change = "dydx"))
ame.mlr = computeAverageMarginalEffects(m, mt.task.small,
  gridsize = getTaskSize(mt.task.small))

# ame and coef of lm are the same
sort(ame)
sort(effects(ame.mlr)[-1])
sort(coefficients(mod)[-1])

################################################################################
# use case 2: in a lm with interactions
################################################################################
f = mpg ~ cyl + hp + (cyl + hp):carb
mt.task$formula = f
m = train(lrn, mt.task)
mod = getLearnerModel(m)

pdat = generatePartialDependenceData(m, mt.task, gridsize = NULL,
  uniform = TRUE, features = c("cyl", "hp", "carb"), method = "simple")
plotPartialDependence(pdat)

ame = summary(margins(mod, data = mtcars))$AME
ame.mlr = computeAverageMarginalEffects(m, mt.task, gridsize = NULL,
  uniform = TRUE, features = c("cyl", "hp", "carb"))

sort(ame)
sort(effects(ame.mlr)[-1])
sort(coefficients(mod)[-1])

################################################################################
# use case 3: in a lm with quadratic effects
################################################################################
f = mpg ~ cyl + I(hp^2) + hp
mt.task$formula = f

m = train(lrn, subsetTask(mt.task, features = c("cyl", "hp")))
mod = getLearnerModel(m)

pdat = generatePartialDependenceData(m, mt.task, gridsize = NULL,
  uniform = FALSE, features = c("hp"), method = "simple")

deriv = generatePartialDependenceData(m, mt.task, gridsize = NULL,
  uniform = FALSE, features = c("hp"), derivative = TRUE,
  method = "simple")

plot(pdat$data$hp, pdat$data$mpg, type = "p")
abline(v = splitPD(x = deriv$data$hp,
  y = deriv$data$mpg, max.splits = 2))

ame = summary(margins(mod, data = mtcars))$AME
# use uniform grid => ame will not be the same due to the non-linear effect
ame.mlr.grid = computeAverageMarginalEffects(m, mt.task, gridsize = NULL,
  uniform = TRUE, features = c("cyl", "hp"))
# we can fix this by NOT using a uniform grid
ame.mlr.data = computeAME(m, mt.task, gridsize = NULL, features = c("cyl", "hp"))

#
sort(ame)
sort(effects(ame.mlr.grid))
sort(effects(ame.mlr.data))

################################################################################
# use case 4: logreg
################################################################################
lrn = makeLearner("classif.logreg", predict.type = "prob")
task = pid.task
f = diabetes ~ pregnant + glucose
task$formula = f

m = train(lrn, task)
mod = getLearnerModel(m)
mod = glm(f, data = getTaskData(task), family = binomial(), x = TRUE)
erer::maBina(w = glm(f, data = getTaskData(task), family = binomial(), x = TRUE),
  x.mean = TRUE, rev.dum = TRUE)
mfx::logitmfx(f, data = getTaskData(task))$mfxest

ame = summary(margins(mod, data = getTaskData(task)))$AME
# we can fix this by NOT using a uniform grid
ame.mlr.data = computeAverageMarginalEffects(m, task, gridsize = NULL,
  uniform = FALSE, features = c("pregnant", "glucose"))
ame.mlr.data2 = computeAME(m, task, gridsize = NULL, features = c("pregnant", "glucose"))

pdat = generatePartialDependenceData(m, task, gridsize = NULL,
  uniform = FALSE, features =  c("pregnant", "glucose"))
plot(pdat$data$glucose, pdat$data$Probability, type = "p")

ame[c("pregnant", "glucose")]
effects(ame.mlr.data)
effects(ame.mlr.data2)

# FIXME: partialDependencePlot just predicts on unique(#values) and not on all training data points

pdat.deriv = generatePartialDependenceData(m, mt.task, c("cyl", "hp"), uniform = FALSE,
  gridsize = getTaskSize(mt.task), derivative = TRUE, method = "simple")
mean(pdat.deriv$data[!is.na(pdat.deriv$data$hp),"mpg"])
weighted.mean(pdat.deriv$data[!is.na(pdat.deriv$data$hp),"mpg"], table(getTaskData(mt.task)$hp))

## testing stuff
# method 1:
derivative = function(model, data, variable, change = "dydx", eps = 1e-8){
  d0 <- d1 <- data
  setstep <- function(x) {
    #x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
    sqrt(eps)*x
  }

  if (change == "dydx") {
    # calculate numerical derivative
    d0[[variable]] <- d0[[variable]] #- setstep(d0[[variable]])
    d1[[variable]] <- d1[[variable]] + setstep(d1[[variable]])
  } else if (change == "minmax") {
    # change from min(x) to max(x)
    d0[[variable]] <- min(d0[[variable]], na.rm = TRUE)
    d1[[variable]] <- max(d1[[variable]], na.rm = TRUE)
  } else if (change == "iqr") {
    # change from fivenum(x)[2] to fivenum(x)[4]
    fnum <- fivenum(d0[[variable]], na.rm = TRUE)
    d0[[variable]] <- fnum[2]
    d1[[variable]] <- fnum[4]
  } else if (change == "sd") {
    # change from mean(x) - sd(x) to mean(x) + sd(x)
    mn <- mean(d0[[variable]], na.rm = TRUE)
    sn <- sd(d0[[variable]], na.rm = TRUE)
    d0[[variable]] <- mn - sn
    d1[[variable]] <- mn + sn
  } else if (change == "numeric") {
    # otherwise `change` was numeric so calculate an arbitrary step
    d0[[variable]] <- lwr
    d1[[variable]] <- upr
  }

  P0 <- prediction(model = model, data = d0, type = "response")[["fitted"]]
  P1 <- prediction(model = model, data = d1, type = "response")[["fitted"]]

  if (change == "dydx") {
    out <- (P1 - P0) / (d1[[variable]] - d0[[variable]])
  } else {
    out <- (P1 - P0)
  }
}
numDerivative = function(f, x, eps = .Machine$double.eps) {
  delta = x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x # x + sqrt(eps)*x
  x1 = x + delta
  (f(x1) - f(x))/(x1 - x)
}
a = derivative(mod, mtcars, c("hp"))
mean(a)

# method 2:
f = function(x, model, data, features) {
  newdata = data
  newdata[features] = x
  mean(prediction(model = model, data = newdata, type = "response")[["fitted"]])
}
mean(vnapply(mtcars$hp, function(x) numDeriv::grad(func = f, x, model =  mod, data = mtcars, features = "hp")))
mean(vnapply(unique(mtcars$hp), function(x) numDeriv::grad(func = f, x, model =  mod, data = mtcars, features = "hp")))

# method 3: here we assume that the cyl values 4,6 and 8 occur with equal frequency in method 1-2 this is not assumed!
lrn = makeLearner("regr.rsm", modelfun = "SO")
m = train(lrn, subsetTask(mt.task, features = c("cyl", "hp")))
pdat = generatePartialDependenceData(m, mt.task, c("cyl", "hp"),
  gridsize = getTaskSize(mt.task), unique.values = TRUE, derivative = TRUE)
head(pdat$data)
mean(pdat$data$mpg[!is.na(pdat$data$cyl)])



ame = computeAverageMarginalEffects(m, pid.task, gridsize = getTaskSize(pid.task))
cbind(BBmisc::vnapply(ame, function(x) x$effects))

library(mfx)
ame.mfx = logitmfx(diabetes~., data =  getTaskData(pid.task))
ame.mfx

library(erer)
mod = glm(diabetes~., data = getTaskData(pid.task), family = binomial, x = TRUE)
maBina(mod)
summary(margins(mod))



x = getTaskData(pid.task, target.extra = TRUE)$data
x = append(list("(Intercept)" = rep(1, 10)), lapply(x, function(x) unique(x)))
# x list of unique features
maBina2 = function(w, x, rev.dum = TRUE, digits = 3, subset.name = NULL,
  subset.value)
{
  if (!inherits(w, "glm")) {
    stop("Need an object from 'glm()'.\n")
  }
  link <- w$family$link
  if (link != "probit" & link != "logit") {
    stop("Need a binary probit or logit model.\n")
  }
  x.bar <- as.matrix(BBmisc::vnapply(x, mean))
  b.est <- as.matrix(coef(w))
  K <- nrow(b.est)
  xb <- t(x.bar) %*% b.est
  pfun <- switch(link, probit = pnorm, logit = plogis)
  dfun <- switch(link, probit = dnorm, logit = dlogis)
  f.xb <- dfun(xb)
  me <- f.xb * coef(w)
  if (link == "probit") {
    s <- -xb
  }
  else {
    s <- 1 - 2 * pfun(xb)
  }
  dr <- c(f.xb) * (diag(1, K, K) + c(s) * (b.est %*% t(x.bar)))
  va <- dr %*% vcov(w) %*% t(dr)
  se <- sqrt(diag(va))
  if (rev.dum) {
    for (i in 1:length(x)) {
      if (identical(sort(unique(x[[i]])), c(0, 1))) {
        x.d1 <- x.bar
        x.d1[i, 1] <- 1
        x.d0 <- x.bar
        x.d0[i, 1] <- 0
        me[i] <- pfun(t(x.d1) %*% b.est) - pfun(t(x.d0) %*%
            b.est)
        dr2 <- dfun(t(x.d1) %*% b.est) %*% t(x.d1) -
          dfun(t(x.d0) %*% b.est) %*% t(x.d0)
        se[i] <- sqrt(c(dr2 %*% vcov(w) %*% t(dr2)))
      }
    }
  }
  out <- data.frame(effect = me, error = se)
  out$t.value <- out$effect/out$error
  out$p.value <-  2 * (1 - pt(abs(out[, 3]), BBmisc::vnapply(x, length) - K))
  #out <- round(out, digits = digits)
  #result <- listn(link, f.xb, w, x, out)
  #class(result) <- "maBina"
  return(out)
}
maBina2(mod, x)



#
m = lm(mpg ~ cyl * hp + wt, data = mtcars)
coefficients(x)
(m <- margins(x))
(marginal_effects(x))

