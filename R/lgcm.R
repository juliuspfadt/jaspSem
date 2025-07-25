#
# Copyright (C) 2013-2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


LatentGrowthCurveInternal <- function(jaspResults, dataset, options, ...) {

  ready <- length(options[["variables"]]) > 2

  # Read dataset
  dataset <- .lgcmReadData(dataset, options)

  # Preprocess options
  options <- .lgcmPreprocessOptions(dataset, options)

  # Enrich dataset
  dataset <- .lgcmEnrichData(dataset, options)

  # Error checking
  .lgcmCheckErrors(dataset, options)

  # Create model container
  modelContainer <- .lgcmModelContainer(jaspResults, options)

  # output
  .lgcmFitTable(            modelContainer, dataset, options, ready )
  .lgcmParameterTables(     modelContainer, dataset, options, ready )
  .lcgmAdditionalFitTables( modelContainer, dataset, options, ready )
  .lcgmRSquaredTable(       modelContainer, dataset, options, ready )
  .lgcmImpliedCovTable(     modelContainer, dataset, options, ready )
  .lgcmResidualCovTable(    modelContainer, dataset, options, ready )
  .lgcmCurvePlot(           modelContainer, dataset, options, ready )
  .lgcmPathPlot(            modelContainer, dataset, options, ready )
  .lgcmSyntax(              modelContainer, dataset, options, ready )
  #.lgcmMisfitPlot(      modelContainer, dataset, options, ready )

}

# Preprocessing functions ----
.lgcmPreprocessOptions <- function(dataset, options) {
  # add dummy names
  if (length(options[["categorical"]]) > 0) {
    frml <- as.formula(paste("~", paste(options[["categorical"]], collapse = "+")))
    dumnames <- colnames(model.matrix(frml, dataset))[-1]
    options[["dummy_names"]] <- dumnames
  }
  return(options)
}

.lgcmReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)
  .readDataSetToEnd(columns = c(
    options[["variables"]],
    options[["regressions"]],
    options[["categorical"]],
    options[["covariates"]]
  ))
}

.lgcmEnrichData <- function(dataset, options) {
  currentNa <- getOption("na.action")
  # Add dummies
  if (length(options[["categorical"]]) > 0) {
    frml <- as.formula(paste("~", paste(options[["categorical"]], collapse = "+")))
    # not error when the categorical variable has NAs:
    options(na.action = "na.pass")
    mm <- model.matrix(frml, dataset)[,-1, drop = FALSE]
    options(na.action = currentNa)
    colnames(mm) <- options[["dummy_names"]]
    dataset <- cbind(dataset, mm)

  }
  return(dataset)
}

.lgcmCheckErrors <- function(dataset, options) {

  if (ncol(dataset) > 0) {
    if (length(options[["models"]]) < 1) return(FALSE)
    usedvars <- unique(unlist(lapply(options[["models"]], function(x) {
      .semGetUsedVars(x[["syntax"]], colnames(dataset))
    })))
    .hasErrors(dataset[,usedvars],
               type = c("infinity"), message='default', exitAnalysisIfErrors = TRUE)
  }

  return()
}

# Results functions ----
.lgcmComputeResults <- function(modelContainer, dataset, options) {

  miss <- if (anyNA(dataset)) options[["naAction"]] else "listwise"

  lgcmResult <- try(lavaan::growth(
    model     = .lgcmOptionsToMod(options),
    data      = dataset,
    se        = ifelse(options[["errorCalculationMethod"]] == "bootstrap", "standard", options[["errorCalculationMethod"]]),
    mimic     = options[["emulation"]],
    estimator = options[["estimator"]],
    missing   = miss
  ))

  if (inherits(lgcmResult, "try-error")) {
    modelContainer$setError(paste(
      "Model error:",
      .decodeVarsInMessage(names(dataset), attr(lgcmResult, "condition")$message))
    )
    return()
  }

  if (!lgcmResult@optim$converged) {
    modelContainer$setError(gettext("The model could not be estimated due to nonconvergence."))
    return()
  }

  if (lgcmResult@test[[1]]$df < 0) {
    modelContainer$setError(gettext("The model could not be estimated: No degrees of freedom left."))
    return()
  }

  # Bootstrapping with interruptible progress bar
  if (options$errorCalculationMethod == "bootstrap") {
    type <- switch(options[["standardizedEstimateType"]],
                   "all" = "std.all",
                   "latents" = "std.lv",
                   "nox" = "std.nox")
    lgcmResult <- lavBootstrap(lgcmResult, samples = options[["bootstrapSamples"]],
                              standard = options[["standardizedEstimate"]],
                              typeStd = type)
  }

  # Save cfaResult as state so it's available even when opts don't change
  modelContainer[["model"]] <- createJaspState(lgcmResult)
  return(lgcmResult)
}

.lgcmOptionsToMod <- function(options, base64 = TRUE) {
  if (!base64) .v <- I
  timings <- sapply(options[["timings"]], function(t) t$timing)

  # Header info
  Hed <- paste0(
    "# -------------------------------------------\n",
    "# Latent Growth Curve model generated by JASP\n",
    "# -------------------------------------------\n"
  )

  # Basic LGCM curve information
  Int <- if (options[["intercept"]])
    paste("I =~", paste0("1*", options[["variables"]], collapse = " + "))
  else NULL
  Lin <- if (options[["linear"]])
    paste("\nL =~", paste0(timings, "*", options[["variables"]], collapse = " + "))
  else NULL
  Qua <- if (options[["quadratic"]])
    paste("\nQ =~", paste0(timings^2, "*", options[["variables"]], collapse = " + "))
  else NULL
  Cub <- if (options[["cubic"]])
    paste("\nC =~", paste0(timings^3, "*", options[["variables"]], collapse = " + "))
  else NULL
  LGC <- paste0("\n# Growth curve\n", Int, Lin, Qua, Cub)

  curve <- c("I", "L", "Q", "C")[with(options, c(intercept, linear, quadratic, cubic))]

  # Covarying latents
  if (!options[["covaryingLatentCurve"]]) {
    Cov <- "\n\n# Suppress latent covariance"
    for (i in seq_along(curve))
      for (j in seq_along(curve))
        if (i < j) Cov <- paste0(Cov, "\n", curve[i], " ~~ 0*", curve[j])
  } else {
    Cov <- NULL
  }

  # Add regressions
  Reg <- if (length(options[["regressions"]]) > 0)
    paste0("\n\n# Regressions\n", paste(curve, collapse = " + "), " ~ ",
           paste(options[["regressions"]], collapse = " + "))
  else NULL

  # Add dummy variables
  Dum <- if (length(options[["dummy_names"]]) > 0)
    paste0("\n\n# Dummy-coded categorical predictors\n", paste(curve, collapse = " + "), " ~ ",
           paste(options[["dummy_names"]], collapse = " + "))

  # Add time-varying covariates
  # TODO

  # Put everything together
  paste0(Hed, LGC, Cov, Reg, Dum)
}

# Output functions ----
.lgcmModelContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c(
      "variables", "regressions", "covariates", "categorical", "timings",
      "intercept", "linear", "quadratic", "cubic", "covaryingLatentCurve",
      "errorCalculationMethod", "bootstrapSamples", "standardizedEstimate",
      "standardizedEstimateType", "naAction", "emulation", "estimator"
    ))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}

.lgcmFitTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["maintab"]])) return()
  maintab <- createJaspTable(gettext("Chi-square test"))
  maintab$addColumnInfo(name = "mod",    title = "Model",        type = "string")
  maintab$addColumnInfo(name = "chisq",  title = "\u03a7\u00b2", type = "number", format = "dp:3")
  maintab$addColumnInfo(name = "df",     title = "df",           type = "integer")
  maintab$addColumnInfo(name = "pvalue", title = "p",            type = "number", format = "dp:3;p:.001")

  modelContainer[["maintab"]] <- createJaspContainer("Model fit")
  modelContainer[["maintab"]]$position <- 1
  modelContainer[["maintab"]][["chisqtab"]] <- maintab

  # add data to the table!
  if (!ready) return()
  lgcmResult <- .lgcmComputeResults(modelContainer, dataset, options)
  if (modelContainer$getError()) return()

  fm <- lavaan::fitMeasures(lgcmResult)
  maintab[["mod"]]    <- c(gettext("Baseline model"), gettext("Growth curve model"))
  maintab[["chisq"]]  <- fm[c("baseline.chisq", "chisq")]
  maintab[["df"]]     <- fm[c("baseline.df", "df")]
  maintab[["pvalue"]] <- c(NA, fm["pvalue"])

  # display warnings in footnote

  admissible <- .withWarnings(lavaan:::lav_object_post_check(lgcmResult))

  if (!admissible$value) {
    maintab$addFootnote(
      message = gettextf(
        "The model is not admissible: %s",
        .decodeVarsInMessage(names(dataset), admissible$warnings[[1]]$message)
      ),
      symbol = gettext("Warning.")
    )
  }

}

.lgcmParameterTables <- function(modelContainer, dataset, options, ready) {
  partabs <- if (!is.null(modelContainer[["partabs"]])) {
    modelContainer[["partabs"]]
  } else {
    modelContainer[["partabs"]] <- createJaspContainer(gettext("Parameter estimates"))
  }
  partabs$dependOn(c("ciLevel", "bootstrapCiType"))
  partabs$position <- 2

  estTitle <- ifelse(options[["standardizedEstimate"]], gettext("Std. estimate"), gettext("Estimate"))

  # create tables
  # latent curve
  latcur <- createJaspTable("Latent curve")
  latcur$addColumnInfo("component", title = gettext("Component"),  type = "string", combine = TRUE)
  latcur$addColumnInfo("param",     title = gettext("Parameter"),  type = "string")
  latcur$addColumnInfo("est",       title = estTitle,   type = "number", format = "dp:3")
  latcur$addColumnInfo("se" ,       title = gettext("Std. error"), type = "number", format = "dp:3")
  latcur$addColumnInfo("zval",      title = gettext("z-value"),    type = "number", format = "dp:3")
  latcur$addColumnInfo("pval",      title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  latcur$addColumnInfo("cilo",      title = gettext("Lower"),      type = "number", format = "dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  latcur$addColumnInfo("ciup",      title = "Upper" ,     type = "number", format = "dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  modelContainer[["partabs"]][["latcur"]] <- latcur

  # covariance
  if (options[["covaryingLatentCurve"]]) {
    latcov <- createJaspTable(gettext("Latent covariances"))
    latcov$addColumnInfo("lhs",  title = "", type = "string")
    latcov$addColumnInfo("sep",  title = "", type = "separator")
    latcov$addColumnInfo("rhs",  title = "", type = "string")
    latcov$addColumnInfo("est",  title = estTitle,   type = "number", format = "dp:3")
    latcov$addColumnInfo("se" ,  title = gettext("Std. error"), type = "number", format = "dp:3")
    latcov$addColumnInfo("zval", title = gettext("z-value"),    type = "number", format = "dp:3")
    latcov$addColumnInfo("pval", title = gettext("p"),          type = "number", format = "dp:3;p:.001")
    latcov$addColumnInfo("cilo", title = gettext("Lower"),      type = "number", format = "dp:3",
                         overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    latcov$addColumnInfo("ciup",      title = "Upper" ,     type = "number", format = "dp:3",
                         overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

    modelContainer[["partabs"]][["latcov"]] <- latcov

  }

  # regressions
  if (length(c(options[["regressions"]], options[["categorical"]])) > 0) {
    latreg <- createJaspTable("Regressions")
    latreg$addColumnInfo("component", title = gettext("Component"),  type = "string", combine = TRUE)
    latreg$addColumnInfo("predictor", title = gettext("Predictor"),  type = "string")
    latreg$addColumnInfo("est",       title = estTitle,   type = "number", format = "dp:3")
    latreg$addColumnInfo("se" ,       title = gettext("Std. error"), type = "number", format = "dp:3")
    latreg$addColumnInfo("zval",      title = gettext("z-value"),    type = "number", format = "dp:3")
    latreg$addColumnInfo("pval",      title = gettext("p"),          type = "number", format = "dp:3;p:.001")
    latreg$addColumnInfo("cilo",      title = gettext("Lower"),      type = "number", format = "dp:3",
                         overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    latreg$addColumnInfo("ciup",      title = "Upper" ,     type = "number", format = "dp:3",
                         overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


    modelContainer[["partabs"]][["latreg"]] <- latreg
  }

  # residual variances
  resvar <- createJaspTable("Residual variances")
  resvar$addColumnInfo("var",  title = gettext("Variable"),   type = "string")
  resvar$addColumnInfo("est",  title = estTitle,   type = "number", format = "dp:3")
  resvar$addColumnInfo("se" ,  title = gettext("Std. error"), type = "number", format = "dp:3")
  resvar$addColumnInfo("zval", title = gettext("z-value"),    type = "number", format = "dp:3")
  resvar$addColumnInfo("pval", title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  resvar$addColumnInfo("cilo", title = gettext("Lower"),      type = "number", format = "dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  resvar$addColumnInfo("ciup",      title = "Upper" ,     type = "number", format = "dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  modelContainer[["partabs"]][["resvar"]] <- resvar

  if (!ready || modelContainer$getError()) return()

  bootstrapCiType <- ifelse(options[["bootstrapCiType"]] == "percentileBiasCorrected", "bca.simple",
                            ifelse(options[["bootstrapCiType"]] == "percentile", "perc",
                                   "norm"))

  #' we need the second option in the if statement because when we require standardized estimates and bootstrapped CIs
  #' the standardization happens in each bootstrap run and the standardized estimates replace the regular raw estimates
  #' in the fit object, and we only need to call parameterEstimates

  fit <- modelContainer[["model"]][["object"]]

  if (!options[["standardizedEstimate"]] ||
      (options[["standardizedEstimate"]] && options[["errorCalculationMethod"]] == "bootstrap")) {
    pe <- lavaan::parameterestimates(fit, level = options[["ciLevel"]],
                                     boot.ci.type = bootstrapCiType)

  } else {
    type <- switch(options[["standardizedEstimateType"]],
                   "all" = "std.all",
                   "latents" = "std.lv",
                   "nox" = "std.nox")
    pe <- lavaan::standardizedSolution(fit, level = options[["ciLevel"]], type = type)
    colnames(pe)[colnames(pe) == "est.std"] <- "est"

  }

  slope_names <- c(
    "^I$" = gettext("Intercept"),
    "^L$" = gettext("Linear slope"),
    "^Q$" = gettext("Quadratic slope"),
    "^C$" = gettext("Cubic slope")
  )

  pe[["lhs"]] <- stringr::str_replace_all(pe[["lhs"]], slope_names)
  pe[["rhs"]] <- stringr::str_replace_all(pe[["rhs"]], slope_names)

  # latent curve
  pecur <- pe[pe$lhs %in% slope_names & (pe$op == "~1" | pe$rhs == pe$lhs),]
  pecur <- pecur[order(pecur$lhs, rev(pecur$op)),]
  latcur[["component"]] <- pecur[["lhs"]]
  latcur[["param"]]     <- ifelse(pecur[["op"]] == "~1", gettext("Mean"), gettext("Variance"))
  latcur[["est"]]       <- pecur[["est"]]
  latcur[["se" ]]       <- pecur[["se"]]
  latcur[["zval"]]      <- pecur[["z"]]
  latcur[["pval"]]      <- pecur[["pvalue"]]
  latcur[["cilo"]]      <- pecur[["ci.lower"]]
  latcur[["ciup"]]      <- pecur[["ci.upper"]]

  # covariance
  if (options[["covaryingLatentCurve"]]) {
    pecov <- pe[pe$lhs %in% slope_names & pe$op == "~~" & pe$lhs != pe$rhs,]
    latcov[["lhs"]]  <- pecov[["lhs"]]
    latcov[["sep"]]  <- rep("\u2B64\u00A0", nrow(pecov))
    latcov[["rhs"]]  <- pecov[["rhs"]]
    latcov[["est"]]  <- pecov[["est"]]
    latcov[["se" ]]  <- pecov[["se"]]
    latcov[["zval"]] <- pecov[["z"]]
    latcov[["pval"]] <- pecov[["pvalue"]]
    latcov[["cilo"]] <- pecov[["ci.lower"]]
    latcov[["ciup"]] <- pecov[["ci.upper"]]
  }

  # regressions
  if (length(c(options[["regressions"]], options[["categorical"]])) > 0) {
    pereg <- pe[pe$lhs %in% slope_names & pe$op == "~",]
    pereg <- pereg[order(pereg$lhs), ]
    latreg[["component"]] <- pereg[["lhs"]]
    latreg[["predictor"]] <- pereg[["rhs"]]
    latreg[["est"]]       <- pereg[["est"]]
    latreg[["se" ]]       <- pereg[["se"]]
    latreg[["zval"]]      <- pereg[["z"]]
    latreg[["pval"]]      <- pereg[["pvalue"]]
    latreg[["cilo"]]      <- pereg[["ci.lower"]]
    latreg[["ciup"]]      <- pereg[["ci.upper"]]
  }

  # residual variances
  perev <- pe[pe$lhs %in% options[["variables"]] & pe$lhs == pe$rhs,]
  resvar[["var"]]  <- perev[["lhs"]]
  resvar[["est"]]  <- perev[["est"]]
  resvar[["se"]]   <- perev[["se"]]
  resvar[["zval"]] <- perev[["z"]]
  resvar[["pval"]] <- perev[["pvalue"]]
  resvar[["cilo"]] <- perev[["ci.lower"]]
  resvar[["ciup"]] <- perev[["ci.upper"]]

  if (any(perev[["est"]] < 0)) {
    resvar$addFootnote(gettext("Residual variance is negative. This may indicate model misspecification."),
                       colNames = "est",
                       rowNames = paste0("row", which(perev[["est"]] < 0) - 1))
  }
}

.lcgmAdditionalFitTables <- function(modelContainer, dataset, options, ready) {

  if (!options[["additionalFitMeasures"]] || !is.null(modelContainer[["fitIndices"]])) return()

  fitinds <- .additionalFitTables(modelContainer, dataset, options, ready)

  fitinds$position <- 3
  modelContainer[["fitIndices"]] <- fitinds

  return()
}

.lcgmRSquaredTable <- function(modelContainer, dataset, options, ready) {
  if (!options[["rSquared"]] || !is.null(modelContainer[["rsquared"]])) return()

  tabr2 <- createJaspTable(gettext("R-Squared"))
  tabr2$position <- 3.5
  tabr2$addColumnInfo(name = "__var__", title = "Variable", type = "string")
  tabr2$addColumnInfo(name = "rsq",     title = "R\u00B2",  type = "number", format = "sf:4;dp:3")
  tabr2$dependOn("rSquared")
  modelContainer[["rsquared"]] <- tabr2

  if (!ready || modelContainer$getError()) return()

  # get r2 of variables, excluding the latent variables.
  r2res <- lavaan::inspect(modelContainer[["model"]][["object"]], "r2")
  r2res <- r2res[!names(r2res) %in% c("I", "L", "Q", "C")]
  varnames <- names(r2res)
  tabr2[["__var__"]] <- varnames
  tabr2[["rsq"]]     <- r2res
}

.lgcmImpliedCovTable <- function(modelContainer, dataset, options, ready) {
  if (!options[["impliedCovariance"]]) return()
  tab <- createJaspTable(gettext("Implied covariance matrix"))
  tab$dependOn("impliedCovariance")
  tab$position <- 4
  modelContainer[["impliedCovTab"]] <- tab

  if (!ready || modelContainer$getError()) return()

  # actually compute the implied covariance
  fv <- lavaan::fitted.values(modelContainer[["model"]][["object"]])
  ic <- fv$cov
  ic[upper.tri(ic)] <- NA

  for (i in 1:ncol(ic)) {
    nm <- colnames(ic)[i]
    tab$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3")
  }
  tab$addRows(ic, rowNames = colnames(ic))

  return()
}

.lgcmResidualCovTable <- function(modelContainer, dataset, options, ready) {
  if (!options[["residualCovariance"]]) return()
  tab <- createJaspTable(gettext("Residual covariance matrix"))
  tab$dependOn("residualCovariance")
  tab$position <- 5
  modelContainer[["rescov"]] <- tab

  if (!ready || modelContainer$getError()) return()

  # actually compute the implied covariance
  rv <- lavaan::residuals(modelContainer[["model"]][["object"]])
  rc <- rv$cov
  rc[upper.tri(rc)] <- NA

  for (i in 1:ncol(rc)) {
    nm <- colnames(rc)[i]
    tab$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3")
  }
  tab$addRows(rc, rowNames = colnames(rc))

  return()
}

.lgcmCurvePlot <- function(modelContainer, dataset, options, ready) {
  if (!options[["curvePlot"]] || !is.null(modelContainer[["curveplot"]])) return()
  curveplot <- createJaspPlot(title = gettext("Curve plot"), width = 480, height = 320)
  curveplot$dependOn(c("curvePlot", "curvePlotCategorical", "curvePlotMaxLines", "colorPalette"))
  curveplot$position <- 8
  modelContainer[["curveplot"]] <- curveplot
  if (!ready || modelContainer$getError()) return()

  lgcmResult <- modelContainer[["model"]][["object"]]
  plt <- .lgcmComputeCurvePlot(lgcmResult, dataset, options)
  curveplot$plotObject <- plt
}

.lgcmComputeCurvePlot <- function(lgcmResult, dataset, options) {
  N   <- lgcmResult@Data@nobs[[1]]
  P   <- length(options[["variables"]])
  idx <- 1:N
  if (N > options[["curvePlotMaxLines"]]) {
    idx <- 1:options[["curvePlotMaxLines"]]
    N   <- options[["curvePlotMaxLines"]]
  }
  ctgcl <- options[["curvePlotCategorical"]] != ""

  # plot the individual-level growth curves
  preds   <- lavaan::lavPredict(lgcmResult)[idx, , drop = FALSE]
  preds   <- cbind(preds, matrix(0, nrow(preds), 4 - ncol(preds)))
  timings <- sapply(options[["timings"]], function(t) t$timing)
  xrange  <- range(timings)
  xx      <- seq(xrange[1], xrange[2], length.out = 1000)
  df_wide <- data.frame(xx = xx, apply(preds, 1, function(b) b[1] + xx*b[2] + xx^2*b[3] + xx^3*b[4]))
  df_long <- tidyr::gather(df_wide, key = "Participant", value = "Val", -"xx")

  if (ctgcl)
    df_long[[options[["curvePlotCategorical"]]]] <- rep(dataset[[options[["curvePlotCategorical"]]]][idx], each = 1000)

  # create raw data points data frame
  points <- data.frame(lgcmResult@Data@X[[1]])[idx, lgcmResult@Data@ov.names[[1]] %in% options[["variables"]]]
  names(points) <- timings
  points[["Participant"]] <- paste0("X", 1:nrow(points))
  points_long <- tidyr::gather(points, key = "xx", value = "Val", -"Participant")
  points_long[["xx"]] <- as.numeric(points_long[["xx"]])

  if (ctgcl)
    points_long[[options[["curvePlotCategorical"]]]] <- rep(dataset[[options[["curvePlotCategorical"]]]][idx],
                                                        length(timings))

  # points may need to be jittered
  jitwidth <- if (N > 30) diff(range(timings) / (15 * P)) else 0
  pos <- ggplot2::position_jitter(width = jitwidth)

  # lines may need to be transparent
  cc <- if (ctgcl) length(unique(points_long[[options[["curvePlotCategorical"]]]])) else 1
  transparency <- min(1, (log(cc) + 1) / log(N))

  # create the plot
  p <-
    ggplot2::ggplot(df_long, ggplot2::aes(x = xx, y = Val, group = Participant)) +
    ggplot2::geom_point(data = points_long, position = pos, size = 2) +
    ggplot2::geom_line(alpha = transparency, size = 0.8) +
    ggplot2::labs(y = "Value", x = "Time") +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::scale_x_continuous() +
    jaspGraphs::scale_y_continuous()

  if (ctgcl)
    return(
      p +
        ggplot2::aes_(colour = as.name(options[["curvePlotCategorical"]]),
                      shape  = as.name(options[["curvePlotCategorical"]])) +
        jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]])
    )

  return(p)
}

.lgcmPathPlot <- function(modelContainer, dataset, options, ready) {
  if (!options$pathPlot || !is.null(modelContainer[["pathplot"]])) return()

  modelContainer[["pathplot"]] <- createJaspPlot(title = gettext("Model plot"), height = 500, width = 640)
  modelContainer[["pathplot"]]$dependOn(c("pathPlot", "pathPlotMean", "pathPlotParameter"))
  modelContainer[["pathplot"]]$position <- 9

  if (!ready || modelContainer$getError()) return()

  lgcmResult <- modelContainer[["model"]][["object"]]
  png() # semplot opens a device even though we specify doNotPlot, so we hack.
  pathplot <- semPlot::semPaths(
    object         = .lavToPlotObj(lgcmResult),
    DoNotPlot      = TRUE,
    ask            = FALSE,
    layout         = "tree2",
    intercepts     = options$pathPlotMean,
    whatLabels     = ifelse(options$pathPlotParameter, "par", "name"),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    border.width   = 1.5,
    edge.label.cex = 0.9,
    lty            = 2,
    title          = FALSE
  )
  dev.off()

  modelContainer[["pathplot"]]$plotObject <- pathplot
}

.lgcmSyntax <- function(modelContainer, dataset, options, ready) {
  if (!ready || !options[["syntax"]]) return()

  modelContainer[["model_syntax"]] <- createJaspHtml(
    text         = .lgcmOptionsToMod(options, FALSE),
    class        = "jasp-code",
    position     = 10,
    title        = "Model Syntax",
    dependencies = "syntax"
  )
}

# Unused functions ----
.lgcmMisfitPlot <- function(modelContainer, dataset, options, ready) {
  if (!options[["misfitPlot"]] || !is.null(modelContainer[["misfitplot"]])) return()
  wh <- 50 + 50*length(options[["variables"]])
  misplot <- createJaspPlot(title = gettext("Misfit plot"), width = wh, height = wh)
  misplot$dependOn("misfitPlot")
  misplot$position <- 9
  modelContainer[["misfitplot"]] <- misplot
  if (!ready || modelContainer$getError()) return()

  lgcmResult <- modelContainer[["model"]][["object"]]
  rescor <- lavaan::residuals(lgcmResult, type = "cor")
  cc <- rescor[["cov"]]
  cc[upper.tri(cc)] <- NA
  gg <- .resCorToMisFitPlot(cc)
  misplot$plotObject <- gg
}

.lgcmPlotRibbon <- function(lgcmResult, options) {
  # plot uncertainty ribbon conditional on regressors == 0

  # get parameter values
  pe <- lavaan::parameterestimates(lgcmResult)
  mu <- pe[pe$lhs %in% c("I", "L", "Q", "C") & pe$rhs == "", ]
  addrow <- matrix(0, 4 - nrow(mu), ncol(mu))
  colnames(addrow) <- names(mu)
  mu <- rbind(mu, addrow)
  s2 <- pe[pe$lhs %in% c("I", "L", "Q", "C") & pe$lhs == pe$rhs,]
  s2 <- rbind(s2, addrow)

  # create x range
  timings <- sapply(options[["timings"]], function(t) t$timing)
  xrange  <- range(timings)
  xx      <- seq(xrange[1], xrange[2], length.out = 1000)

  # inner ribbon (with only parameter uncertainty, no variance)
  # growth curve for a typical person with covariates at 0
  mu_mu <- mu$est[1] + xx*mu$est[2] + xx^2*mu$est[3] + xx^3*mu$est[4]
  mu_up <- mu$ci.upper[1] + xx*mu$ci.upper[2] + xx^2*mu$ci.upper[3] + xx^3*mu$ci.upper[4]
  mu_lo <- mu$ci.lower[1] + xx*mu$ci.lower[2] + xx^2*mu$ci.lower[3] + xx^3*mu$ci.lower[4]


  # growth curve for draws as if our group were the population
  mp <- qnorm(0.975)
  s2_up <- (mu$est[1] + mp*sqrt(s2$est[1])) +
    xx   * (mu$est[2] + mp*sqrt(s2$est[2])) +
    xx^2 * (mu$est[3] + mp*sqrt(s2$est[3])) +
    xx^3 * (mu$est[4] + mp*sqrt(s2$est[4]))
  s2_lo <- (mu$est[1] - mp*sqrt(s2$est[1])) +
    xx   * (mu$est[2] - mp*sqrt(s2$est[2])) +
    xx^2 * (mu$est[3] - mp*sqrt(s2$est[3])) +
    xx^3 * (mu$est[4] - mp*sqrt(s2$est[4]))

  # growth curve for groups of people with covariates 0 and parameter uncertainty
  ms_up <- (mu$ci.upper[1] + mp*sqrt(s2$ci.upper[1])) +
    xx   * (mu$ci.upper[2] + mp*sqrt(s2$ci.upper[2])) +
    xx^2 * (mu$ci.upper[3] + mp*sqrt(s2$ci.upper[3])) +
    xx^3 * (mu$ci.upper[4] + mp*sqrt(s2$ci.upper[4]))
  ms_lo <- (mu$ci.lower[1] - mp*sqrt(s2$ci.upper[1])) +
    xx   * (mu$ci.lower[2] - mp*sqrt(s2$ci.upper[2])) +
    xx^2 * (mu$ci.lower[3] - mp*sqrt(s2$ci.upper[3])) +
    xx^3 * (mu$ci.lower[4] - mp*sqrt(s2$ci.upper[4]))

  fac <- forcats::fct_rev(forcats::as_factor(rep(c("mu", "s2", "ms"), each = 1000)))

  df    <- data.frame(xx = rep(xx, 3), up = c(mu_up, s2_up, ms_up), lo = c(mu_lo, s2_lo, ms_lo), which = fac)
  mu_df <- data.frame(xx = xx, y = mu_mu)

  # create raw data points data frame
  points <- data.frame(lgcmResult@Data@X[[1]])[idx, lgcmResult@Data@ov.names[[1]] %in% options[["variables"]]]
  names(points) <- timings
  points[["Participant"]] <- paste0("X", 1:nrow(points))
  points_long <- tidyr::gather(points, key = "xx", value = "Val", -"Participant")
  points_long[["xx"]] <- as.numeric(points_long[["xx"]])

  # points may need to be jittered
  jitwidth <- if (N > 30) diff(range(timings) / (15 * P)) else 0
  pj <- ggplot2::position_jitter(width = jitwidth)

  ggplot2::ggplot(df, mapping = ggplot2::aes(x = xx)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymax = up, ymin = lo, fill = which)) +
    ggplot2::geom_line(data = mu_df, col = "#454545", ggplot2::aes(y = y)) +
    ggplot2::geom_point(ggplot2::aes(y = Val), data = points_long, position = pj, col = "#454545") +
    ggplot2::scale_fill_manual(
      values = c("#ABABAB", "#909090", "#777777"),
      guide  = 'legend',
      labels = c(gettext("Curve + variance + uncertainty"),
                 gettext("Curve + variance"),
                 gettext("Curve + parameter uncertainty"))
    ) +
    labs(fill = "", x = "Time", y = "Value") +
    theme_minimal()
}
