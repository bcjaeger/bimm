

bimm_init <- function(data_train,
                      formula,
                      verbose,
                      n_iteration,
                      fun_model_ml = NULL,
                      fun_model_mer = NULL,
                      fun_pred_ml = NULL,
                      fun_pred_mer = NULL){

  if(is.null(fun_model_ml))
    fun_model_ml <- function(formula, data_train){

      ranger::ranger(
        formula = formula,
        data = data_train,
        probability = TRUE
      )

    }

  if(is.null(fun_pred_ml))
    # TODO: make new_data = NULL default, work in cases
    fun_pred_ml <- function(model, new_data = NULL){

      col_index <- which(model$forest$class.values==1)

      if(is.null(new_data)){
        return(getElement(model, "predictions")[, col_index])
      }

      stats::predict(model, data = new_data)$predictions[, col_index]

    }

  if(is.null(fun_model_mer))
    fun_model_mer <- function(formula, data_train){

      lme4::glmer(formula = formula,
                  data = data_train,
                  family = stats::binomial)

    }

  if(is.null(fun_pred_mer))
    fun_pred_mer <- function(model, new_data){

      stats::predict(model,
                     newdata = new_data,
                     re.form = NULL, # include all random effects
                     type = 'response')
    }

  .formula <- formula_expand(data = data_train, formula = formula)

  history <- data.frame(
    iteration = seq(n_iteration),
    loglik    = NA_real_,
    auc_train = NA_real_,
    auc_test  = NA_real_,
    bri_train = NA_real_,
    bri_test  = NA_real_
  )

  pseudo_outcome <- data.frame(
    init = data_train[[.formula$outcome]]
  )

  structure(
    .Data = list(colname_outcome = .formula$outcome,
                 colname_id = .formula$id,
                 formula_ml = .formula$ml,
                 formula_mer = .formula$mer,
                 history = history,
                 pseudo_outcome = pseudo_outcome,
                 fun_model_ml = fun_model_ml,
                 fun_model_mer = fun_model_mer,
                 fun_pred_ml = fun_pred_ml,
                 fun_pred_mer = fun_pred_mer,
                 model_ml = NULL,
                 model_mer = NULL,
                 verbose = verbose,
                 iteration_current = 1,
                 n_iteration = n_iteration),
    class = 'bimm_object'
  )

}


#' @export
print.bimm_object <- function(x, ...){

  cat("Binary mixed model predicting", x$colname_outcome)

  cat("\n\n----- ML model formula: ----- \n\n",
      trimws(deparse(x$formula_ml)),
      sep = '')

  cat("\n\n----- Mixed effects regression formula: ----- \n\n",
      sub(pattern = '._x_.',
          replacement = 'predict(ML model)',
          fixed = TRUE,
          x =  trimws(deparse(x$formula_mer))),
      sep = '')

  cat("\n\n----- Model history: ----- \n\n")
  print(x$history)

}
