# Example usecase for OOP:
# objects representing regression models that are used for prediction.
# This is in parts similar to the 'mlr3' package (github.com/mlr-org/mlr3)
# that gets developed at LMU. After looking at these examples you are
# invited to look at the code of the mlr3-package, but that code handles
# many more cases than we do here, so it is much more complicated.

library("checkmate")
library("R6")
library("data.table")

################################################
# S3 example:
# Represent possible hyperparameters of methods: "ParamSpace" / "Param".

# Param: represent a single "hyperparameter", or more specifically, its range of valid values.
# This in itself doesn't do much, it serves as a base-class for the classes that follow.
Param <- function() {
  structure(list(), class = "Param")
}

assertParam <- function(param, value) {
  UseMethod("assertParam")
}

## True / False Param

# Doesn't do much either, but it shows how to inherit from Param:
PBool <- function() {
  p <- Param()
  class(p) <- c("PBool", class(p))
  p
}

# nice printer
print.PBool <- function(x, ...) {
  cat("{T, F}\n")
  invisible(x)  # a print.<###> function should always return 'invisible(x)'.
}

# assertParam for bools: check that 'value' is True / False (using assertFlag from checkmate)
assertParam.PBool <- function(param, value) {
  assertFlag(value)
}

## Factorial Param
# Param that accepts a string, from a choice of options ('levels').
# The naming is according to ?factor in R.
PFct <- function(levels) {
  p <- Param()
  # This is how a constructor looks like if it expands the object.
  # E.g. here: add the '$levels' slot
  p$levels <- assertCharacter(levels, min.len = 1, unique = TRUE, any.missing = FALSE)
  class(p) <- c("PFct", class(p))
  p
}

# nice printer: print only 3 levels; if there are more than 3 levels, print '...' at the end:
print.PFct <- function(x, ...) {
  lrep <- head(x$levels, 3)
  if (length(x$levels) > 3) lrep[[3]] <- "..."
  cat(sprintf("{%s}\n", paste(lrep, collapse = ", ")))
  invisible(x)
}

assertParam.PFct <- function(param, value) {
  assertChoice(value, param$levels)
}

## (Continuous) Numeric Param

PNum <- function(lower = -Inf, upper = Inf) {
  p <- Param()
  p$lower <- assertNumber(lower)
  p$upper <- assertNumber(upper, lower = lower)
  class(p) <- c("PNum", class(p))
  p
}

# print the inclusive bounds using [<lower>, <upper>]
print.PNum <- function(x, ...) {
  cat(sprintf("[%s, %s]\n", x$lower, x$upper))
  invisible(x)
}

assertParam.PNum <- function(param, value) {
  assertNumber(value, lower = param$lower, upper = param$upper)
}

## Integer Param

# not many surprises here...
PInt <- function(lower = -Inf, upper = Inf) {
  p <- Param()
  # - assert that bounds are integer values
  # - but *also* accept infinite bounds (i.e. unbounded)
  assert(checkInt(lower, tol = 1e-100), checkTRUE(identical(lower, -Inf)))
  p$lower <- lower
  assert(checkInt(upper, lower = lower, tol = 1e-100), checkTRUE(identical(upper, Inf)))
  p$upper <- upper
  class(p) <- c("PInt", class(p))
  p
}

# complicated printer: we inject '...' in different places, depending on the bounds
print.PInt <- function(x, ...) {
  if (x$lower == -Inf && x$upper == Inf) {
    cat("{..., -1, 0, 1, ...}\n")
  } else if (x$lower == -Inf) {
    cat(sprintf("{..., %s}\n", paste(seq(to = x$upper, length.out = 3), collapse = ", ")))
  } else if (x$upper == Inf) {
    cat(sprintf("{%s, ...}\n", paste(seq(from = x$lower, length.out = 3), collapse = ", ")))
  } else if (x$upper - x$lower < 3) {
    cat(sprintf("{%s}\n", paste(seq(from = x$lower, to = x$upper), collapse = ", ")))
  } else {
    cat(sprintf("{%s, ..., %s}\n", x$lower, x$upper))
  }
  invisible(x)
}

assertParam.PInt <- function(param, value) {
  assertInt(value, lower = param$lower, upper = param$upper, tol = 1e-100)
}

# Set of Hyperparamters: ParamSpace
# This is a named lists of Param-objects internally
ParamSpace <- function(...) {
  pp <- list(...)  # get a list from function arguments, see ?dots
  assertList(pp, names = "unique", types = "Param")
  structure(pp, class = "ParamSpace")
}

# printer: just call print() for the contained params
# (with a prefixed ' * ' so it looks nice)
print.ParamSpace <- function(x, ...) {
  if (length(x) == 0) {
    cat("Empty ParamSpace.\n")
    return(invisible(x))
  }
  cat("ParamSpace:\n")
  for (pn in names(x)) {
    cat(sprintf(" * %s: ", pn))
    print(x[[pn]])
  }
  invisible(x)
}

# call assert() for each Param and the corresponding value
# (but accept that some values may be absent)
assertParam.ParamSpace <- function(param, value) {
  assertList(value, names = "unique")
  assertNames(names(value), subset.of = names(param))
  for (pn in names(value)) {
    assertParam(param[[pn]], value[[pn]])
  }
}

ps <- ParamSpace(x = PInt(0, 10), y = PFct(letters))
assertParam(ps, list(x = 2, y = "n"))

################################################

# - provide the '$train()' and '$predict() method: a unified interface for fitting models.
# - "abstract base class": should not be created by itself, instead should only be inherited from
# - does the work that other classes have in common; an inheriting class must only
#   implement their own model fitting code

Estimator <- R6Class("Estimator",
                     public = list(
                       # the constructor gets the ParamSpace that pertains to the method.
                       initialize = function(param.space) {
                         private$.param.space <- assertClass(param.space, "ParamSpace")
                         # initialize private$.configuration to list().
                         # alternatively, we could have initialized in the "private" block
                         # further down; doing it here is a matter of convention.
                         private$.configuration <- list()
                       },
                       train = function(data, target) {
                         # do checks etc and call 'private$.train()' (the abstract method).
                         # - inheriting classes must implement the 'private$.train()' method
                         # - but inheriting classes don't need to worry about checks!
                         assertDataFrame(data, min.rows = 1, min.cols = 2, col.names = "unique")
                         assertChoice(target, colnames(data))
                         # save the trained model
                         private$.model <- private$.train(as.data.frame(data), target)
                         # also remember the columns of the data we trained on.
                         private$.trained.features <- setdiff(colnames(data), target)
                         private$.trained.target <- target
                         
                         # Why do we return invisible(self) here?
                         # because the train() method is a "mutator" function: it changes the
                         # object itself. If a method does that (and has no other reasonable
                         # return value), then returning invisible(self) gives the option of
                         # "chaining" methods:
                         # One can call <object>$train(data, target)$predict(newdata)
                         # and because '$train()' returns the object itself, the '$predict()' call
                         # does what one would expect here.
                         # Why 'invisible'? Because calling just <object>$train(data, target) should
                         # not give any output. If we don't have the 'invisible()' here, then
                         # returning just 'self' would result in the object being printed.
                         invisible(self)
                       },
                       predict = function(data) {
                         # same as train: do the common work, then call the abstract function (private$.predict)
                         if (is.null(self$model)) stop("Estimator is not trained yet.")
                         
                         assertDataFrame(data, col.names = "unique")
                         assertNames(setdiff(colnames(data), private$.trained.target),
                                     permutation.of = private$.trained.features)
                         
                         data <- as.data.frame(data)  # in case we have a data.table here
                         # put columns in the order as during training, possibly remove target col if present
                         data <- data[private$.trained.features]
                         prediction <- private$.predict(data)
                         # check that the inheriting class did everything right. Just like
                         # function arguments, it is good to check here because we get the prediction
                         # from "untrusted" code.
                         assertNumeric(prediction, any.missing = FALSE, len = nrow(data))
                         prediction
                       },
                       print = function(x, ...) {
                         # R6 printer is another public method called 'print'.
                         cat(class(self)[[1]])
                         if (!is.null(self$model)) cat(" (trained)")
                         cat("\n")
                         print(self$param.space)
                         # We don't need to worry about returning invisible(x) for R6.
                       }
                     ),
                     active = list(
                       # Active bindings: we use this to make some member variables "read-only"
                       param.space = function(v) {
                         if (!missing(v)) stop("param.space is read-only")
                         private$.param.space
                       },
                       # the 'configuration' active binding uses the assertParam() functionality
                       # of the given ParamSpace to check that the configuration is valid.
                       configuration = function(v) {
                         if (!missing(v)) {
                           assertParam(self$param.space, v)
                           private$.configuration <- v
                         }
                         private$.configuration
                       },
                       # another read-only active binding.
                       model = function(v) {
                         if (!missing(v)) stop("model is read-only")
                         private$.model
                       }
                     ),
                     private = list(
                       .param.space = NULL,
                       .configuration = NULL,
                       .model = NULL,
                       .trained.features = NULL,
                       .trained.target = NULL,
                       .train = function(data, target) stop(".train must be implemented"),
                       .predict = function(data) stop(".predict must be implemented")
                     )
)

# helper function: create the formula from given target name
# getDataFormula("Sepal.Length") --> Sepal.Length ~ .
getDataFormula <- function(target) {
  # not relevant for this course unit, but a 'good' R programmer
  # should see what is happening here.
  eval(substitute(target ~ ., list(target = as.symbol(target))))
}

##################
# Concrete classes inheriting from the abstract Estimator class

# linear model class
EstimatorLm <- R6Class("EstimatorLm",
                       inherit = Estimator,
                       public = list(
                         # - The concrete classes have constructors without arguments.
                         # - Instead, they call super$initialize() with the ParamSpace that is
                         #   relevant for them.
                         initialize = function() {
                           # (linear model has no hyperparameters --> empty ParamSpace)
                           super$initialize(ParamSpace())
                         }
                       ),
                       private = list(
                         .train = function(data, target) {
                           # training the model: call the lm() function
                           lm(getDataFormula(target), data)
                         },
                         .predict = function(data) {
                           # predicting: use predict() method for the given model.
                           unname(predict(self$model, newdata = data))
                         }
                       )
)

# decision tree ("rpart" package) class
EstimatorRpart <- R6Class("EstimatorRpart",
                          inherit = Estimator,
                          public = list(
                            initialize = function() {
                              # only some of the many hyperparameters of ?rpart::rpart.control
                              super$initialize(ParamSpace(
                                # lower and upper bound of PInt / PNum are default infinite. Giving only
                                # the 'lower' argument here is equivalent to PInt(2, Inf).
                                minsplit = PInt(2),
                                minbucket = PInt(1),
                                cp = PNum(0, 1)
                              ))
                            }
                          ),
                          private = list(
                            .train = function(data, target) {
                              # configuring the rpart::rpart hyperparameters is done using the
                              # ?rpart::rpart.control object.
                              config <- self$pconfiguration
                              # we want to set rpart.control$xval to 0, because we don't need xval
                              # if we only need to predict.
                              config$xval <- 0
                              # here we create the rpart.control object, using ?do.call
                              control <- do.call(rpart::rpart.control, config)
                              # and here we create the rpart model
                              rpart::rpart(getDataFormula(target), data, control = control)
                            },
                            .predict = function(data) {
                              # predict() for rpart works the same as for lm()
                              unname(predict(self$model, newdata = data))
                            }
                          )
)

# random forest ("ranger" package) class
EstimatorRanger <- R6Class("EstimatorRanger",
                           inherit = Estimator,
                           public = list(
                             initialize = function() {
                               # for demo, only few of the *many* hyperparameters of ranger
                               super$initialize(ParamSpace(
                                 mtry = PInt(1),
                                 num.trees = PInt(1)
                               ))
                             }
                           ),
                           private = list(
                             .train = function(data, target) {
                               # - we call ranger::ranger(<formula>, <data>, {self$configuration})
                               # - for this we use do.call, as in EstimatorRpart
                               # - therefore we need to insert the formula and data into a named list
                               #   which we then give to do.call().
                               args <- self$configuration
                               args$formula <- getDataFormula(target)
                               args$data <- data
                               model <- do.call(ranger::ranger, args)
                               
                               
                               # (we overwrite the '$call' slot of the model here because
                               # do.call() makes things very ugly otherwise when we print the model...
                               # the next line is very peculiar and you don't need to know why this is
                               # happening...)
                               model$call <- substitute(ranger::ranger(data, target), list())
                               
                               model
                             },
                             .predict = function(data) {
                               # prediction works *differently* from lm() prediction: we need
                               # to return the $predictions slot of the result of predict() here:
                               predict(self$model, data = data)$predictions
                             }
                           )
)

# we can also use S3 methods for R6 class.
# this is nice here, because this way we also follow the pattern in R that
# models can be used for prediction using predict(<model>, newdata = <data>).
predict.Estimator <- function(object, newdata, ...) {
  object$predict(newdata)
}


#######################################
# What can we use the above for?
# We can, for example, write a function that estimates the
# "root mean squared error" (RMSE) that we get when we train a model on 2/3
# of the data, and predict on the remaining 1/3.
#
# The 'holdoutResample()' function does not need to know what method is used,
# it only needs to know that it can use $train() to train a model, and use
# $predict() to do prediction.

# See chapter 4 of the online LMU course "i2ml"
# (<https://introduction-to-machine-learning.netlify.app/>)
# to know more about this.
holdoutResample <- function(estimator, data, target) {
  assertDataFrame(data, min.rows = 3)
  assertClass(estimator, "Estimator")
  trainset <- sample(nrow(data), nrow(data) * 2 / 3)
  traindata <- data[trainset, ]
  testdata <- data[-trainset, ]
  predictions <- estimator$train(traindata, target)$predict(testdata)
  sqrt(mean((data[[target]] - predictions)^2))
}

# We can use this, for example, to benchmark different methods on a dataset:

estimators <- list()
estimators$lm <- EstimatorLm$new()
estimators$rpart.default <- EstimatorRpart$new()
estimators$rpart.configured <- EstimatorRpart$new()
estimators$rpart.configured$configuration$cp <-0.0001
estimators$ranger <- EstimatorRanger$new()

# make a benchmark of the different methods:
# holdoutResample() gets called 40 times for each method, the result
# is saved in a data.table that we can then plot
results <- rbindlist(lapply(names(estimators), function(ne) {
  est <- estimators[[ne]]
  data.table(
    method = ne,
    performance = replicate(40, holdoutResample(est, iris, "Sepal.Width")
    ))
}))
# (we could turn this into a function!
# This would then be similar to the 'benchmark()' function in mlr3.)

library("ggplot2")
ggplot(results, aes(x = method, y = performance)) + geom_boxplot()