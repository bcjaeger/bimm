


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

  p <- predict(object = object$model_ml,
                 data = new_data,
                 type = 'response')

  if(type == 'new_sub'){
    return(p$predictions[, 2, drop=TRUE])
  }

  if(type == 'new_obs'){

    test_MLprob <- data.frame(._x_. = p$predictions[, 2, drop=TRUE])

    predict(object = object$model_mer,
            newdata = cbind(test_MLprob, new_data),
            type="response",
            re.form = NULL,
            allow.new.levels=TRUE)

  }

}
