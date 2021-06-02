
formula_get_outcome_vars <- function(formula){
  all.vars(stats::update(formula, . ~ 1))
}


formula_get_fixed_vars <- function(formula){

  no_bars <- lme4::nobars(formula)

  all.vars(stats::update(no_bars, 1 ~ .))

}


formula_get_random_expr <- function(formula) {

  formula_random <- lme4::findbars(formula)

  str_random_pieces <- unlist(lapply(formula_random, deparse))

  str_random_embraced <- paste('(', str_random_pieces, ')', sep = '')

  paste(str_random_embraced, collapse = ' + ')

}



formula_get_random_vars <- function(formula) {

  unique(unlist(lapply(lme4::findbars(formula), all.vars)))

}

formula_get_id_vars <- function(formula){

  strings_random <-
    unlist(lapply(lme4::findbars(formula), deparse))

  strings_id <- lapply(
    X = strsplit(strings_random, split = '|', fixed = TRUE),
    FUN = trimws
  )

  sapply(strings_id, function(x) x[length(x)])

}


formula_expand <- function(formula, data){

  .formula <- stats::formula(x = stats::terms(formula, data = data))

  outcome <- formula_get_outcome_vars(.formula)
  vars_random <- formula_get_random_vars(.formula)
  expr_random <- formula_get_random_expr(.formula)

  vars_id <- formula_get_id_vars(.formula)
  vars_fixed <- formula_get_fixed_vars(.formula)
  # dont let the repeated measure ID's be used in RF models
  vars_fixed <- setdiff(vars_fixed, vars_random)
  expr_fixed <- paste(vars_fixed, collapse = ' + ')

  formula_ml <- stats::as.formula(paste(outcome, expr_fixed, sep = ' ~ '))
  formula_mer <- stats::as.formula(paste(outcome,'~ ._x_. + ', expr_random))

  list(
    outcome = outcome,
    id = vars_id,
    ml = formula_ml,
    mer = formula_mer
  )

}
