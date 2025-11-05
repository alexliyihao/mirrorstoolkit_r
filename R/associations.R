utils::globalVariables(c("Effect Size"))
#' Ordinary Linear Regression
#'
#' A wrapper creating linear regression formula and clean the output
#'
#' @param data data.frame the table working on
#' @param variable str, the name of column of interested variable in table data
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param mode str, when "summary", only output the coefficient corresponding to column specified in variable
#' @return data.frame, if mode summary, only the beta and p-value for variable specified in "variable", otherwise the general summary()
#' @export
linear_regression = function(data, variable, response, adjustments, mode = "table"){
  if (variable %in% adjustments){
    adjustments = adjustments[!adjustments %in% variable]
  }
  output = stats::lm(stats::reformulate(
    response = response,
    c(variable, adjustments, 1)),
    data = data,
  )
  if (mode == "summary"){
    return(output %>% summary())
  } else{
    result = (output %>% summary())$coefficients %>% as.data.frame()
    return(result[variable,c("Estimate", "Pr(>|t|)")])
  }
}

#' Ordinary Linear Regression with multi variable of interest
#'
#' A wrapper running multiple linear_regression() for a list of variables of interest
#'
#' @param data data.frame the table working on
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param variable_of_interest vector of str, the names of columns of interested variable in table data
#' @param variable_of_interest_formal_name vector of str, the output names of interested variable in result
#' @return data.frame, one variable in variable_of_interest as a row, with it's beta and p-value
OLS_wrapper_ = function(data, response, adjustments, variable_of_interest, variable_of_interest_formal_name){
  result = list()
  for (i in variable_of_interest){
    result[[i]] = linear_regression(
      variable = i,
      data = data,
      adjustments = adjustments,
      response = response,
      mode = "table")
  }
  result_table = base::do.call(rbind, result) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("Effect Size", "P-value")) %>%
    magrittr::set_rownames(variable_of_interest_formal_name)
  return(result_table)
}

#' Ordinary Linear Regression wrapper with multi variable of interest and stratification
#'
#' A wrapper running multiple linear_regression() for a list of variables of interest, stratification function provided
#' @importFrom rlang .data
#' @param data data.frame the table working on
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param by str, the name of column of stratified variable in table data
#' @param variable_of_interest vector of str, the names of columns of interested variable in table data
#' @param variable_of_interest_formal_name optional vector of str, the output names of interested variable in result
#'
#' @return data.frame, one variable in variable_of_interest as a row, with it's beta and p-value
#' @export
OLS_wrapper = function(data, response, adjustments, variable_of_interest, by=NULL, variable_of_interest_formal_name = NULL){

  if (is.null(variable_of_interest_formal_name)){
    variable_of_interest_formal_name = variable_of_interest
  }

  if (is.null(by)){
    return(OLS_wrapper_(
      response = response,
      data = data,
      adjustments = adjustments,
      variable_of_interest = variable_of_interest,
      variable_of_interest_formal_name = variable_of_interest_formal_name))
  }
  if (by %in% adjustments){
    adjustments_ = adjustments[!adjustments %in% by]
  }
  output = data %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(by))) %>%
    dplyr::do(OLS_wrapper_(
      response = response,
      data = .data,
      adjustments = adjustments_,
      variable_of_interest = variable_of_interest,
      variable_of_interest_formal_name = variable_of_interest_formal_name) %>%
        tibble::rownames_to_column("variable"))
  return(output)
}

#' Logistics Linear Regression
#'
#' A wrapper creating logistic regression formula and clean the output
#'
#' @param data data.frame the table working on
#' @param variable str, the name of column of interested variable in table data
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param mode str, when "summary", only output the coefficient corresponding to column specified in variable
#' @return data.frame, if mode summary, only the beta and p-value(z-value) for variable specified in "variable", otherwise the general summary()
#' @export
logistic_regression = function(data, variable, response, adjustments, mode = "table"){
  if (variable %in% adjustments){
    adjustments = adjustments[!adjustments %in% variable]
  }
  output = stats::glm(stats::reformulate(
    response = response,
    c(variable, adjustments, 1)),
    data = data,
    stats::binomial(link = "logit")
  )
  if (mode == "summary"){
    return(output %>% summary())
  } else{
    result = (output %>% summary())$coefficients %>% as.data.frame()
    return(result[variable,c("Estimate", "Pr(>|z|)")])
  }
}

#' Logistics Linear Regression with multi-variable of interest
#'
#' A wrapper running multiple logistics_regression() for a list of variables of interest
#'
#' @param data data.frame the table working on
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param variable_of_interest vector of str, the names of columns of interested variable in table data
#' @param variable_of_interest_formal_name vector of str, the output names of interested variable in result
#' @return data.frame, one variable in variable_of_interest as a row, with it's beta and p-value
logistic_wrapper_ = function(data, response, adjustments, variable_of_interest,variable_of_interest_formal_name){
  result = list()
  for (i in variable_of_interest){
    result[[i]] = logistic_regression(
      variable = i,
      data = data,
      adjustments = adjustments,
      response = response,
      mode = "table")
  }

  result_table = base::do.call(rbind, result) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("Effect Size", "P-value")) %>%
    magrittr::set_rownames(variable_of_interest_formal_name)
  return(result_table)
}

#' Logistics Linear Regression wrapper with multi variable of interest and stratification
#'
#' A wrapper running multiple linear_regression() for a list of variables of interest, stratification function provided
#' @importFrom rlang .data
#' @param data data.frame the table working on
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param by str, the name of column of stratified variable in table data
#' @param variable_of_interest vector of str, the names of columns of interested variable in table data
#' @param variable_of_interest_formal_name vector of str, the output names of interested variable in result
#' @return data.frame, one variable in variable_of_interest as a row, with it's beta and p-value
#' @export
logistic_wrapper = function(data, response, adjustments, by = NULL, variable_of_interest = NULL,variable_of_interest_formal_name = NULL){

  if (is.null(variable_of_interest_formal_name)){
    variable_of_interest_formal_name = variable_of_interest
  }

  if (is.null(by)){
    output = logistic_wrapper_(
      response = response,
      data = data,
      adjustments = adjustments,
      variable_of_interest = variable_of_interest,
      variable_of_interest_formal_name = variable_of_interest_formal_name)%>%
      dplyr::mutate(`Odd Ratio` = base::exp(`Effect Size`))%>%
      dplyr::select(c("Effect Size","Odd Ratio","P-value"))
    return(output)
  }
  if (by %in% adjustments){
    adjustments_ = adjustments[!adjustments %in% by]
  }
  output = data %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(by))) %>%
    dplyr::do(logistic_wrapper_(
      response = response,
      data = .data,
      adjustments = adjustments_,
      variable_of_interest = variable_of_interest,
      variable_of_interest_formal_name = variable_of_interest_formal_name) %>%
        tibble::rownames_to_column("variable")
    ) %>%
    dplyr::mutate(`Odd Ratio` = base::exp(`Effect Size`)) %>%
    dplyr::select(c("variable","Effect Size","Odd Ratio","P-value"))
  return(output)
}
