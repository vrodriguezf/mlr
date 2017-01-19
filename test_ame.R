library(data.table)
library(BBmisc)
# see also https://diffuseprior.wordpress.com/2012/04/23/probitlogit-marginal-effects-in-r-2/

load_all()

# use case 1: in a lm without interactions, the ame should equate to the coefficients!
mt.task = makeRegrTask(data = mtcars, target = "mpg")
lrn = makeLearner("regr.lm")
m = train(lrn, mt.task)
ame = computeAverageMarginalEffects(m, mt.task, gridsize = getTaskSize(mt.task))
vnapply(ame, function(x) x$effects)
coefficients(m$learner.model)[-1]

# compare with margins github package
library("margins")
mod = lm(mpg~., data = mtcars)
summary(margins(mod))

# use case 2: in a lm with interactions
library("margins")
mod = lm(mpg ~ cyl + hp + I(cyl^2) + I(hp^2) + cyl:hp, data = mtcars)
coefficients(mod)
(m <- margins(mod))
# FIXME: Effects are colMeans(meff) -> check what happens there
meff <- marginal_effects(mod, change = "sd")
cplot(mod, x = c("hp"))
out = cplot(mod, x = "cyl")

# FIXME: partialDependencePlot just predicts on unique(#values) and not on a continous grid
lrn = makeLearner("regr.rsm", modelfun = "SO")
m = train(lrn, subsetTask(mt.task, features = c("cyl", "hp")))
coefficients(m$learner.model)[-1]
pdat = generatePartialDependenceData(m, mt.task, getTaskFeatureNames(mt.task), gridsize = getTaskSize(mt.task), unique.values = FALSE)
plotPartialDependence(pdat)
ame = computeAverageMarginalEffects(m, mt.task, gridsize =10, features = c("cyl", "hp"))
vnapply(ame, function(x) x$effects)

cplot(mod, x = c("cyl"))
lines(pdat$data$cyl, pdat$data$mpg, col ="red")
cplot(mod, x = c("hp"))
lines(pdat$data$hp, pdat$data$mpg, col ="red")

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

a = derivative(mod, mtcars, c("cyl"))
mean(a)

# method 2:
f = function(x, model, data, features) {
  newdata = data
  newdata[features] = x
  mean(prediction(model = model, data = newdata, type = "response")[["fitted"]])
}
mean(vnapply(mtcars$cyl, function(x) grad(func = f, x, model =  mod, data = mtcars, features = "cyl")))
mean(vnapply(unique(mtcars$cyl), function(x) grad(func = f, x, model =  mod, data = mtcars, features = "cyl")))


# method 3: here we assume that the cyl values 4,6 and 8 occur with equal frequency in method 1-2 this is not assumed!
lrn = makeLearner("regr.rsm", modelfun = "SO")
m = train(lrn, subsetTask(mt.task, features = c("cyl", "hp")))
pdat = generatePartialDependenceData(m, mt.task, getTaskFeatureNames(mt.task),
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
maBina2 = function (w, x, rev.dum = TRUE, digits = 3, subset.name = NULL,
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

