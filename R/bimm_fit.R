

#' @title Fit Binary Mixed Model (BiMM)
#'
#' @description BiMMs are designed to form predictions for clustered outcomes
#'   using standard machine learning algorithms in combination with mixed
#'   effects regression models. The BiMM algorithm is as follows:
#'
#'     1. Fit a machine learning model to the specified outcome variable.
#'        Assume observations are independent.
#'
#'     2. Calculate predicted probabilities for all observations.
#'
#'     3. Fit a mixed effects regression model that accounts for correlations
#'         among clusters and uses the predicted probabilities as the only
#'         fixed effect.
#'
#'     4. Update the outcome variable using predicted probabilities from
#'        the binary mixes model. Use all estimated random effects in the
#'        predicted values. Apply a splitting function to dichotomize
#'        the predicted probabilities.
#'
#'     5. Repeat steps 1-4 until convergence criteria are met.
#'
#'  Predictions for observations included within the training data are made
#'  using mixed effects regression model. For observations in testing data, predictions
#'  are made using only the machine learning model.
#'
#' @param data a data frame containing the variables in the model.
#'
#' @param formula An object of class "formula" that is akin to `formula` in
#'   [lme4::lmer()] where both fixed and random effects are written.
#'   Random-effects terms are distinguished by vertical bars (|) inside of
#'   parentheses. See examples.
#'
#' @param verbose An integer value of 0, 1, or 2.
#'  - If verbose = 0, nothing is printed to console.
#'  - If verbose = 1, select steps are printed to console.
#'  - If verbose = 2, almost all processes are printed to console.
#'
#' @param n_iteration A positive integer indicating how many iterations
#'   to complete. More details on 'iterations' can be found in the 'Details'
#'   section.
#'
#' @param fun_model_ml a function to fit the machine learning model
#'   during the iterative bimm procedure. If `NULL`, a default function
#'   will be used.
#'
#' @param fun_model_mer a function to fit the mixed effects regression
#'   model during the iterative bimm procedure. If `NULL`, a default
#'   function will be used.
#'
#' @param fun_pred_ml a function to compute predicted values from the
#'   machine learning model. If `NULL`, a default function will be used.
#'
#' @param fun_pred_mer a function to compute predicted values from the
#'   mixed effects regression model. If `NULL`, a default function
#'   will be used.
#'
#' @return An object of class `bimm_object` with the following components:
#'
#' - `colname_outcome`: the name of the outcome variable.
#'
#' - `colname_id`: the name of the cluster identifier
#'
#' - `formula_ml`: the formula passed to the machine learning model
#'
#' - `formula_mer`: the formula passed to the mixed effects regression model.
#'
#' - `history`: a dataframe showing convergence metrics for each iteration.
#'    Each row represents the convergence metrics at a specific iteration.
#'
#' - `pseudo_outcome`: a dataframe containing pseudo outcome values for
#'    each iteration. Each column represents the pseudo outcome at a
#'    specific iteration.
#'
#' - `fun_model_ml`: function used to fit the machine learning model.
#'
#' - `fun_model_mer`: function used to fit the mixed effects regression model.
#'
#' - `fun_pred_ml`: function used to compute predictions with the
#'    machine learning model.
#'
#' - `fun_pred_mer`: function used to compute predictions with the
#'    mixed effects regrssion model.
#'
#' - `model_ml`: the machine learning model
#'
#' - `model_mer`: the mixed effects regression model
#'
#' - `verbose`: the verbosity of the `bimm_object`.
#'
#' - `iteration_current` the current iteration of the `bimm_object`
#'
#' - `n_iteration` the total number of iterations.
#'
#' @export
#'
#' @examples
#'
#' bimm_fit(data = hospital[1:1000, ],
#'          formula = remission ~ . + (1 | DID),
#'          verbose = TRUE,
#'          n_iteration = 2)
#'
bimm_fit <- function(data,
                     formula,
                     verbose,
                     n_iteration,
                     fun_model_ml = NULL,
                     fun_model_mer = NULL,
                     fun_pred_ml = NULL,
                     fun_pred_mer = NULL){

  bimm_object <- bimm_init(data_train = data,
                           formula = formula,
                           verbose = verbose,
                           n_iteration = n_iteration,
                           fun_model_ml = fun_model_ml,
                           fun_model_mer = fun_model_mer,
                           fun_pred_ml = fun_pred_ml,
                           fun_pred_mer = fun_pred_mer)

  bimm_iterate(data_train = data, object = bimm_object)

}


