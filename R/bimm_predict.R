


#' Title
#'
#' @param object
#' @param new_data
#' @param type
#'
#' @return
#' @export
#'
#' @examples
#'
bimm_predict <- function(object,
                         new_data,
                         type = 'new_obs'){

  if(type == 'new_sub'){
    p <- predict(object = object$model_ml,
                 data = new_data,
                 type = 'response')
    return(p$predictions[, 2, drop=TRUE])
  }

  if(type == 'new_obs'){

    tmp <- data.frame(._x_. = object$fun_pred_ml(object$model_ml))

    predict(object = object$model_mer,
            newdata = cbind(tmp, new_data),
            type="response",
            re.form = NULL,
            allow.new.levels=TRUE)

  }

}
