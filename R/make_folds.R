


make_folds <- function(data, id, n_fold){

  id_unique <- base::unique(data[[id]])

  folds <- sample(n_fold,
                  size = length(id_unique),
                  replace = TRUE)

  output <- data.frame(x = id_unique, fold = folds)

  names(output)[1] <- id

  output


}
