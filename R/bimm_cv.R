
#' @title Cross-validation for BiMMs
#'
#' @inheritParams bimm_fit
#'
#' @param n_fold an integer value of 2 or higher indicating how many folds
#'   should be created to run cross-validation. More folds => more computation
#'   time. Less folds => higher variability in performance assessments.
#'
#' @param evaluation_metric A character value indicating which evaluation
#'   metric to track as iterations are completed. Valid options are 'auc'
#'   for the area underneath the receiver operator characteristic curve
#'   and 'bri' for the Brier score.
#'
#' @return a dataframe with estimated evaluation metrics at each iteration
#'   of the bimm fitting procedure.
#'
#' @export
#'
bimm_cv <- function(data,
                    formula,
                    verbose,
                    n_fold = 10,
                    n_iteration = 5,
                    evaluation_metric = 'auc'){


  .formula <- formula_expand(data = data, formula = formula)

  .folds <- make_folds(data = data, id = .formula$id, n_fold = n_fold)

  # hold the testing indices in this list
  test_index <- lapply(
    X = seq(n_fold),
    FUN = function(x){
      fold_ids <- .folds[[.formula$id]][.folds$fold == x]
      which(data[[.formula$id]] %in% fold_ids)
    }
  )

  # initialize list to hold the models
  models <- vector(mode = 'list', length = n_fold)

  # initialize the models
  for(fold in seq(n_fold)){

    models[[fold]] <-
      bimm_init(data_train = data[-test_index[[fold]], ],
                formula = formula,
                verbose = FALSE,
                n_iteration = 1)
  }

  # create a dataframe to store history from cross-validation

  history <- data.frame(
    iteration = seq(n_iteration),
    auc_train = NA_real_,
    bri_train = NA_real_,
    auc_test = NA_real_,
    bri_test = NA_real_
  )

  iteration_cv <- 1

  repeat {

    if(verbose) message("Iteration ", iteration_cv, appendLF = FALSE)

    for(fold in seq(n_fold)){

      if(verbose) message(".", appendLF = FALSE)

      models[[fold]] <-
        bimm_iterate(data_train = data[-test_index[[fold]], ],
                     data_test = data[test_index[[fold]], ],
                     object = models[[fold]],
                     keep_model_mer = FALSE,
                     keep_model_ml = FALSE)

    }

    h_cols <- c('auc_train', 'bri_train', 'auc_test', 'bri_test')

    h <- Reduce(
      f = rbind,
      x = lapply(models, function(x)
        x$history[x$history$iteration == iteration_cv, h_cols]
      )
    )

    history[history$iteration == iteration_cv, h_cols] <- apply(h, 2, mean)

    if(verbose){

      stats <- lapply(
        X = history[history$iteration == iteration_cv, h_cols],
        FUN = function(x) format(round(x, digits = 3), nsmall = 3)
      )

      switch(
        evaluation_metric,
        'auc' = message("train AUC: ", stats$auc_train, "\t",
                        "test AUC:", stats$auc_test),
        'bri' = message("train Brier: ", stats$bri_train, "\t",
                        "test Brier:", stats$bri_test)
      )

    }

    if(iteration_cv == n_iteration) break

    iteration_cv <- iteration_cv + 1

  }

  # concatenate history from all models

  history

}














