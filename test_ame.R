library(data.table)
library(BBmisc)
library(devtools)
# see also https://diffuseprior.wordpress.com/2012/04/23/probitlogit-marginal-effects-in-r-2/

load_all()
#mtcars$carb = as.factor(mtcars$carb)
mt.task = makeRegrTask(data = mtcars, target = "mpg")

################################################################################
# use case 1: in a lm without interactions, the ame should equate to the coefficients!
################################################################################
lrn = makeLearner("regr.lm")
m = train(lrn, mt.task)
ame = computeAverageMarginalEffects(m, mt.task, gridsize = getTaskSize(mt.task))
vnapply(ame, function(x) x$effects)
coefficients(m$learner.model)[-1]

# compare with margins github package https://github.com/leeper/margins
library("margins")
mod = lm(mpg ~., data = mtcars)
colMeans(marginal_effects(mod, change = "dydx"))
margins(mod)

################################################################################
# use case 2: in a lm with interactions
################################################################################
library("margins")
f = mpg ~ cyl + hp + (cyl + hp):carb
mod = lm(f, data = mtcars)
coefficients(mod)
margins(mod, type = "response")
par(mfrow = c(2,2))
cplot(mod, x = "cyl")
cplot(mod, x = "hp")
cplot(mod, x = "carb")

mt.task$formula = f
m = train(lrn, subsetTask(mt.task, features = c("cyl", "hp", "carb")))
pdat = generatePartialDependenceData(m, mt.task, c("cyl", "hp", "carb"), method = "simple")
plotPartialDependence(pdat)
ame = computeAverageMarginalEffects(m, mt.task, gridsize = 100, features = c("cyl", "hp", "carb"))
vnapply(ame, function(x) x$effects)
coefficients(m$learner.model)[-1]

par(mfrow = c(2,2))
cplot(mod, x = c("cyl"))
lines(pdat$data$cyl, pdat$data$mpg, col = "red")
cplot(mod, x = c("hp"))
lines(pdat$data$hp, pdat$data$mpg, col = "red")
cplot(mod, x = c("carb"))
lines(pdat$data$carb, pdat$data$mpg, col = "red")
# FIXME: findout why intercept differs!


################################################################################
# use case 3: in a lm with quadratic effects
################################################################################
library("margins")
f = mpg ~ cyl + I(cyl^2) + hp
mod = lm(f, data = mtcars)
coefficients(mod)
margins(mod, type = "response")
par(mfrow = c(1,2))
cplot(mod, x = "cyl")
cplot(mod, x = "hp")

mt.task$formula = f
m = train(lrn, subsetTask(mt.task, features = c("cyl", "hp")))
pdat = generatePartialDependenceData(m, mt.task, c("cyl", "hp"), method = "simple")
plotPartialDependence(pdat)
ame = computeAverageMarginalEffects(m, mt.task, gridsize = 100, features = c("cyl", "hp"))
vnapply(ame, function(x) x$effects) # not equal to margins(mod, type = "response")
# FIXME: partialDependencePlot just predicts on unique(#values) and not on all data points

pdat.deriv = generatePartialDependenceData(m, mt.task, c("cyl", "hp"),
  gridsize = getTaskSize(mt.task), derivative = TRUE, method = "simple")
weighted.mean(pdat.deriv$data[!is.na(pdat.deriv$data$cyl),"mpg"], table(getTaskData(mt.task)$cyl))


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
    out <- (P1 - P0)#/ (d1[[variable]] - d0[[variable]])
  }
}
numDerivative = function(f, x, eps = .Machine$double.eps) {
  delta = x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x # x + sqrt(eps)*x
  x1 = x + delta
  (f(x1) - f(x))/(x1 - x)
}
a = derivative(mod, mtcars, c("cyl"))
mean(a)

# method 2:
f = function(x, model, data, features) {
  newdata = data
  newdata[features] = x
  mean(prediction(model = model, data = newdata, type = "response")[["fitted"]])
}
mean(vnapply(mtcars$cyl, function(x) numDeriv::grad(func = f, x, model =  mod, data = mtcars, features = "cyl")))
mean(vnapply(unique(mtcars$cyl), function(x) numDeriv::grad(func = f, x, model =  mod, data = mtcars, features = "cyl")))

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

