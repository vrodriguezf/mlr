library(data.table)
library(BBmisc)
library(devtools)
# see also https://diffuseprior.wordpress.com/2012/04/23/probitlogit-marginal-effects-in-r-2/

load_all()
mtcars$cyl = as.factor(mtcars$cyl)

f = mpg ~ cyl + hp + (cyl + hp):carb
mt.task = makeRegrTask(data = mtcars, target = "mpg")
mt.task$formula = f

grid.size = getTaskSize(mt.task)
dat = getTaskData(mt.task, target.extra = TRUE)

# features for pd
feats = c("hp", "carb")

# fit model
lrn = makeLearner("regr.lm")
m = train(lrn, subsetTask(mt.task, features = c("cyl", "hp", "carb")))
fit = getLearnerModel(m)

# use mlr
pdat = generatePartialDependenceData(m, mt.task, gridsize = grid.size, uniform = TRUE,
  features = feats, method = "simple")

# use zachs package
library("mmpf")
mmpf = setNames(lapply(feats, function(f) {
  mp = marginalPrediction(getTaskData(mt.task, target.extra = TRUE)$data,
    f, c(grid.size, grid.size), fit, uniform = TRUE)
}), feats)

# use pdp
library("pdp")
mod = lm(f, data = mtcars)
pdp = setNames(lapply(feats, function(f) {
  partial(mod, pred.var = f, grid.resolution = grid.size)
}), feats)

# use ice
library("ICEbox")
ice = setNames(lapply(feats, function(f) {
  ice = ice(fit,
    X = dat$data,
    y = dat$target,
    predictor = f, num_grid_pts = grid.size)
}), feats)

library("margins")
margins = setNames(lapply(feats, function(f) {
  cplot(mod, x = f)
}), feats)

for (i in feats) {
  cplot(mod, x = i)
  lines(margins[[i]]$xvals, margins[[i]]$yvals, col = "pink")
  lines(pdat$data[[i]], pdat$data$mpg, col = "red")
  lines(mmpf[[i]][[i]], mmpf[[i]]$preds, col = "blue")
  lines(pdp[[i]][[i]], pdp[[i]]$yhat, col = "green")
  lines(ice[[i]]$gridpts, ice[[i]]$pdp, col = "yellow")
}
# now let's plot the slope using the ame
ame.effect = colMeans(marginal_effects(mod, change = "dydx"))
abline(coef = c(20, ame.effect["dydx_carb"]))

# FIXME: findout why curve from margins differ!
