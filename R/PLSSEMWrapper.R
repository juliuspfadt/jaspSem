#
# Copyright (C) 2013-2025 University of Amsterdam
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
#

# This is a generated file. Don't change it!

#' PLSSEM
#'
PLSSEM <- function(
          data = NULL,
          version = "0.95",
          addConstructScores = FALSE,
          additionalFitMeasures = FALSE,
          benchmark = "none",
          bootstrapSamples = 200,
          ciLevel = 0.95,
          consistentPartialLeastSquares = TRUE,
          convergenceCriterion = "absoluteDifference",
          endogenousIndicatorPrediction = FALSE,
          errorCalculationMethod = "bootstrap",
          group = list(types = "unknown", value = ""),
          handlingOfInadmissibles = "replace",
          impliedConstructCorrelation = FALSE,
          impliedIndicatorCorrelation = FALSE,
          innerWeightingScheme = "path",
          kFolds = 10,
          mardiasCoefficient = FALSE,
          models = list(list(name = "Model", syntax = list(columns = list(), model = "", modelOriginal = ""))),
          observedConstructCorrelation = FALSE,
          observedIndicatorCorrelation = FALSE,
          omfBootstrapSamples = 499,
          omfSignificanceLevel = 0.05,
          overallModelFit = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          rSquared = FALSE,
          reliabilityMeasures = FALSE,
          repetitions = 10,
          saturatedStructuralModel = FALSE,
          seed = 1,
          setSeed = FALSE,
          structuralModelIgnored = FALSE,
          tolerance = 1e-05) {

   defaultArgCalls <- formals(jaspSem::PLSSEM)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("convergenceCriterion", "group", "innerWeightingScheme", "models")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem", "PLSSEM", "PLSSEM.qml", options, version, TRUE))
}