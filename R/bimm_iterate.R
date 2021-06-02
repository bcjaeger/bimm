

bimm_iterate <- function(data_train,
                         data_test = NULL,
                         object,
                         keep_model_mer = TRUE,
                         keep_model_ml = TRUE) {

  # convert the outcome column in the training data to the most
  # recent pseudo-outcome in the bimm training process.

  if(object$iteration_current > 1)
    data_train[[object$colname_outcome]] <-
      prob_split_stochastic(
        x = data_train[[object$colname_outcome]] +
          object$pseudo_outcome[[object$iteration_current]],
        cut_lower = 0.5,
        cut_upper = 1.5,
        probs = object$pseudo_outcome[[object$iteration_current]]
      )


  # if object has already iterated to its maximum, make room for one more.
  # note, the second logical is there to prevent this happening when
  # the bimm_object hasn't finished 1 iteration yet
  if(object$iteration_current == object$n_iteration &&
     !is.na(object$history$loglik[object$iteration_current])){

    object$iteration_current <- object$iteration_current + 1
    object$n_iteration <- object$n_iteration + 1

    new_history <- data.frame(iteration = object$iteration_current,
                              loglik    = NA_real_,
                              auc_train = NA_real_,
                              auc_test  = NA_real_,
                              bri_train = NA_real_,
                              bri_test  = NA_real_)

    object$history <- rbind(object$history, new_history)

  }

  repeat {

    if(object$verbose)
      message("Beginning iteration ",
              object$iteration_current, "...",
              appendLF = FALSE)

    object$model_ml <- object$fun_model_ml(formula = object$formula_ml,
                                           data_train = data_train)

    # note that setting new_data = NULL returns out of bag predictions
    # for the training data. Is this a good thing?
    data_train[['._x_.']] <- object$fun_pred_ml(model = object$model_ml,
                                                new_data = NULL)

    object$history[object$iteration_current, 'auc_train'] <-
      auc_compute(pred = data_train$._x_.,
                  resp = data_train[[object$colname_outcome]])

    object$history[object$iteration_current, 'bri_train'] <-
      brier_compute(pred = data_train$._x_.,
                    resp = data_train[[object$colname_outcome]])

    if(!is.null(data_test)){

      data_test[['._x_.']] <- object$fun_pred_ml(model = object$model_ml,
                                                 new_data = data_test)

      object$history[object$iteration_current, 'auc_test'] <-
        auc_compute(pred = data_test$._x_.,
                    resp = data_test[[object$colname_outcome]])

      object$history[object$iteration_current, 'bri_test'] <-
        brier_compute(pred = data_test$._x_.,
                      resp = data_test[[object$colname_outcome]])

    }

    object$model_mer <- try(
      object$fun_model_mer(formula = object$formula_mer,
                           data_train = data_train),
      silent = TRUE
    )

    if(inherits(object$model_mer, 'try-error')){
      message("mixed effects model could not be fit; ",
              "ending on iteration ", object$iteration_current)
      break
    }

    object$history[object$iteration_current, 'loglik'] <-
      as.numeric(stats::logLik(object$model_mer))



    mer_probs <- object$fun_pred_mer(model = object$model_mer,
                                     new_data = data_train)

    if(object$verbose) message("Done!")

    new_pseudo_name <- paste0("iter_", object$iteration_current)

    object$pseudo_outcome[[new_pseudo_name]] <- mer_probs

    if(object$iteration_current == object$n_iteration) break

    data_train[[object$colname_outcome]] <-
      prob_split_stochastic(
        x = data_train[[object$colname_outcome]] + mer_probs,
        cut_lower = 0.5,
        cut_upper = 1.5,
        probs = mer_probs
      )

    object$iteration_current <- object$iteration_current + 1

  }

  if(!keep_model_ml)
    object$model_ml <- 'deleted b/c keep_model_ml is FALSE'

  if(!keep_model_mer)
    object$model_mer <- 'deleted b/c keep_model_mer is FALSE'

  object

}
