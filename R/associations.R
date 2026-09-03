utils::globalVariables(c("Effect Size"))
#' Ordinary Linear Regression
#'
#' @description A wrapper creating linear regression formula and clean the output
#'
#' @param data data.frame the table working on
#' @param variable str, the name of column of interested variable in table data
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param mode str, when "summary", only output the coefficient corresponding to column specified in variable
#' @param with_power boolean, when set to TRUE, will call power.t.regression function computing the power and sample size at 80 percent power, default FALSE to save computation burden
#' @param variable_distribution str, either "normal" or "binary", the distribution of variable of interest specified, only necessary when with_power = TRUE, default "normal"
#'
#' @return data.frame, if mode summary, only the beta and p-value for variable specified in "variable", otherwise the general summary()
#'
#' @export
linear_regression = function(
    data,
    variable,
    response,
    adjustments,
    mode = "table",
    with_power = FALSE,
    variable_distribution = "normal"){
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
  }
  result = (output %>% summary())$coefficients %>% as.data.frame()
  ci <- stats::confint(output, level = 0.95)
  output_with_CI = result[variable, c("Estimate", "Pr(>|t|)")] %>%
    cbind(
      CI_lower = ci[variable, 1],
      CI_upper = ci[variable, 2]
    )
  if (with_power == TRUE){
    # prepare the distribution for power analysis
    cleaned_result = broom::glance(output)
    nobs = cleaned_result %>% dplyr::pull("nobs")
    r_squared = cleaned_result %>% dplyr::pull("r.squared")
    beta = result[variable,c("Estimate")]
    sd_outcome = data %>% dplyr::pull(response) %>% sd(na.rm = TRUE)
    if (variable_distribution == "normal"){
      sd_predictor = data %>% dplyr::pull(variable) %>% sd(na.rm = TRUE)
    } else if (variable_distribution == "binary") {
      p_predictor = data %>% dplyr::pull(variable) %>% mean(na.rm = TRUE)
      sd_predictor = base::sqrt(p_predictor * (1 - p_predictor))
    } else {
      base::stop("the distribution of the variable not supported")
    }
    current_power = pwrss::power.t.regression(
      beta = beta,
      sd.predictor = sd_predictor,
      sd.outcome = sd_outcome,
      r.squared = r_squared,
      k.total = (adjustments %>% length()) + 1,
      alpha = 0.05,
      n = nobs,
      #power = 0.80
      )
    size_at_eighty_pct = pwrss::power.t.regression(
      beta = beta,
      sd.predictor = sd_predictor,
      sd.outcome = sd_outcome,
      r.squared = r_squared,
      k.total = (adjustments %>% length()) + 1,
      alpha = 0.05,
      #n = nobs,
      power = 0.80,
      ceiling = TRUE)
    return(c(
      output_with_CI,
      current_power$power,
      nobs,
      size_at_eighty_pct$n))
  }
  else{
    return(output_with_CI)
  }
}

#' Ordinary Linear Regression with multi variable of interest
#'
#' @description A wrapper running multiple linear_regression() for a list of variables of interest
#'
#' @param data data.frame the table working on
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param variable_of_interest vector of str, the names of columns of interested variable in table data
#' @param variable_of_interest_formal_name vector of str, the output names of interested variable in result
#' @param with_power boolean, when set to TRUE, will call power.t.regression function computing the power and sample size at 80 percent power, default FALSE to save computation burden
#' @param variable_distribution vector of str, either "normal" or "binary", the distribution of variable of interest specified, only necessary when with_power = TRUE, default "normal", should be 1-1 corresponding to variable of interest
#'
#' @return data.frame, one variable in variable_of_interest as a row, with it's beta and p-value
OLS_wrapper_ = function(
    data,
    response,
    adjustments,
    variable_of_interest,
    variable_of_interest_formal_name,
    with_power = TRUE,
    variable_distribution = NULL){
  result = list()
  for (i in seq(1, length(variable_of_interest))){
    result[[variable_of_interest[i]]] = linear_regression(
      variable = variable_of_interest[i],
      data = data,
      adjustments = adjustments,
      response = response,
      mode = "table",
      with_power = with_power,
      variable_distribution = variable_distribution[i])
  }
  result_table = base::do.call(rbind, result) %>% as.data.frame()
  if (with_power == TRUE){
    result_table = result_table %>%
      tidyr::unnest(
        result_table %>% colnames(),
        keep_empty = TRUE
      ) %>%
      magrittr::set_colnames(
        c("Effect Size", "P-value", "CI_low", "CI_high", "current power", "n_observations","80%_power_size")
      ) %>%
      as.data.frame() %>%
      magrittr::set_rownames(variable_of_interest_formal_name)
  } else {
    result_table = result_table %>%
      magrittr::set_colnames(
        c("Effect Size", "P-value", "CI_low", "CI_high")
      ) %>%
      magrittr::set_rownames(variable_of_interest_formal_name)
  }
  return(result_table)
}

#' Ordinary Linear Regression wrapper with multi variable of interest and stratification
#'
#' @description A wrapper running multiple linear_regression() for a list of variables of interest, stratification function provided
#' @importFrom rlang .data
#' @param data data.frame the table working on
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param by str, the name of column of stratified variable in table data
#' @param variable_of_interest vector of str, the names of columns of interested variable in table data
#' @param variable_of_interest_formal_name optional vector of str, the output names of interested variable in result
#' @param with_power boolean, when set to TRUE, will call power.t.regression function computing the power and sample size at 80 percent power, default FALSE to save computation burden
#' @param variable_distribution vector of str, either "normal" or "binary", the distribution of variable of interest specified, only necessary when with_power = TRUE, default "normal", should be 1-1 corresponding to variable of interest
#'
#' @return data.frame, one variable in variable_of_interest as a row, with it's beta and p-value
#'
#' @export
OLS_wrapper = function(
    data,
    response,
    adjustments,
    variable_of_interest,
    by=NULL,
    variable_of_interest_formal_name = NULL,
    with_power = FALSE,
    variable_distribution = NULL){

  if (is.null(variable_of_interest_formal_name)){
    variable_of_interest_formal_name = variable_of_interest
  }

  if (is.null(by)){
    return(OLS_wrapper_(
      response = response,
      data = data,
      adjustments = adjustments,
      variable_of_interest = variable_of_interest,
      variable_of_interest_formal_name = variable_of_interest_formal_name,
      with_power = with_power,
      variable_distribution = variable_distribution))
  }
  if (by %in% adjustments){
    adjustments_ = adjustments[!adjustments %in% by]
  }
  output = data %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(by))) %>%
    dplyr::group_modify(
      ~ OLS_wrapper_(
      response = response,
      data = .data,
      adjustments = adjustments_,
      variable_of_interest = variable_of_interest,
      variable_of_interest_formal_name = variable_of_interest_formal_name,
      with_power = with_power,
      variable_distribution = variable_distribution) %>%
        tibble::rownames_to_column("variable"),
      .keep = TRUE)
  return(output)
}

#' Logistics Linear Regression
#'
#' @description A wrapper creating logistic regression formula and clean the output
#'
#' @param data data.frame the table working on
#' @param variable str, the name of column of interested variable in table data
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param mode str, when "summary", only output the coefficient corresponding to column specified in variable
#' @param with_power boolean, when set to TRUE, will call pwrss.z.logreg function computing the power and sample size at 80 percent power, default FALSE to save computation burden
#' @param variable_distribution str, either "normal" or "binary", the distribution of variable of interest specified, only necessary when with_power = TRUE, default "normal"
#'
#' @return data.frame, if mode summary, only the beta and p-value(z-value) for variable specified in "variable", otherwise the general summary()
#'
#' @export
logistic_regression = function(
    data,
    variable,
    response,
    adjustments,
    mode = "table",
    with_power = FALSE,
    variable_distribution = "normal"){
  if (variable %in% adjustments){
    # clean the redundant variables
    adjustments = adjustments[!adjustments %in% variable]
  }
  # Run logistic regression
  output = stats::glm(stats::reformulate(
    response = response,
    c(variable, adjustments, 1)),
    data = data,
    stats::binomial(link = "logit")
  )
  if (mode == "summary"){
    return(output %>% summary())
  }
  result = (output %>% summary())$coefficients %>% as.data.frame()
  ci <- stats::confint(output, level = 0.95)
  output_with_CI = result[variable, c("Estimate", "Pr(>|z|)")] %>%
    cbind(
      OR = exp(result[variable, "Estimate"]),
      CI_lower = exp(ci[variable, 1]),
      CI_upper = exp(ci[variable, 2])
    )
  if (with_power == TRUE){
    # Compute correlation with other x for power analysis
    p0 = (data %>% dplyr::pull(response) %>% table() %>% prop.table())[2]
    other_x_correlation = (stats::lm(stats::reformulate(
      response = variable,
      c(adjustments, 1)),
      data = data) %>%
        summary()
    )$adj.r.squared
    # prepare the distribution for power analysis
    nobs = broom::glance(output) %>% dplyr::pull("nobs")
    beta1 = result[variable,c("Estimate")]
    if (variable_distribution == "normal"){
      dist_for_power = list(
        dist = "normal",
        mean = data %>% dplyr::pull(variable) %>% mean(na.rm = TRUE),
        sd = data %>% dplyr::pull(variable) %>% sd(na.rm = TRUE)
        )
    } else if (variable_distribution == "binary") {
      dist_for_power = list(
        dist = "bernoulli",
        prob = data %>% dplyr::pull(variable) %>% mean(na.rm = TRUE)
        )
    } else {
      base::stop("the distribution of the variable not supported")
    }
    # The pwrss.z.logistic() and its alias pwrss.z.logreg() are deprecated.
    # However, they will remain available as wrappers for the power.z.logistic() function.
    # For better version versatility I kept it as-is
    current_power = pwrss::pwrss.z.logreg(
      p0 = p0,
      beta1 = beta1,
      r2.other.x = max(other_x_correlation, 0),
      alpha = 0.05,
      n = nobs,
      #power = 0.80,
      dist = dist_for_power)
    size_at_eighty_pct = pwrss::pwrss.z.logreg(
      p0 = p0,
      beta1 = beta1,
      r2.other.x = max(other_x_correlation, 0),
      alpha = 0.05,
      #n = nobs,
      power = 0.80,
      dist = dist_for_power)
    return(c(
      output_with_CI,
      current_power$power,
      nobs,
      (size_at_eighty_pct$n) %>% ceiling()))
  } else{
    return(output_with_CI)
  }
}

#' Logistics Linear Regression with multi-variable of interest
#'
#' @description A wrapper running multiple logistics_regression() for a list of variables of interest
#'
#' @param data data.frame the table working on
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param variable_of_interest vector of str, the names of columns of interested variable in table data
#' @param variable_of_interest_formal_name vector of str, the output names of interested variable in result
#' @param with_power boolean, when set to TRUE, will call pwrss.z.logreg function computing the power and sample size at 80 percent power, default FALSE to save computation burden
#' @param variable_distribution vector of str, either "normal" or "binary", the distribution of variable of interest specified, only necessary when with_power = TRUE, default "normal", should be 1-1 corresponding to variable of interest
#'
#' @return data.frame, one variable in variable_of_interest as a row, with it's beta and p-value
logistic_wrapper_ = function(
    data,
    response,
    adjustments,
    variable_of_interest,
    variable_of_interest_formal_name,
    with_power,
    variable_distribution){
  result = list()
  for (i in seq(1, length(variable_of_interest))){
    result[[variable_of_interest[i]]] = logistic_regression(
      variable = variable_of_interest[i],
      data = data,
      adjustments = adjustments,
      response = response,
      mode = "table",
      with_power = with_power,
      variable_distribution = variable_distribution[i])
  }
  result_table = base::do.call(rbind, result) %>% as.data.frame()
  if (with_power == TRUE){
    result_table = result_table %>%
      tidyr::unnest(
        result_table %>% colnames(),
        keep_empty = TRUE
        ) %>%
      magrittr::set_colnames(
        c("Effect Size", "P-value",
          "OR", "CI_lower", "CI_upper",
          "current power", "n_observations","80%_power_size")
        ) %>%
      as.data.frame() %>%
      magrittr::set_rownames(variable_of_interest_formal_name)
  } else {
    result_table = result_table %>%
      magrittr::set_colnames(
        c("Effect Size", "P-value","OR", "CI_lower", "CI_upper")
        ) %>%
      magrittr::set_rownames(variable_of_interest_formal_name)
  }
  return(result_table)
}

#' Logistics Linear Regression wrapper with multi variable of interest and stratification
#'
#' @description A wrapper running multiple linear_regression() for a list of variables of interest, stratification function provided
#' @importFrom rlang .data
#' @param data data.frame the table working on
#' @param response str, the name of column of target variable in table data
#' @param adjustments vector of str, the list of adjustment variables in the table data
#' @param by str, the name of column of stratified variable in table data
#' @param variable_of_interest vector of str, the names of columns of interested variable in table data
#' @param variable_of_interest_formal_name vector of str, the output names of interested variable in result
#' @param with_power boolean, when set to TRUE, will call pwrss.z.logreg function computing the power and sample size at 80 percent power, default FALSE to save computation burden
#' @param variable_distribution vector of str, either "normal" or "binary", the distribution of variable of interest specified, only necessary when with_power = TRUE, default "normal", should be 1-1 corresponding to variable of interest
#'
#' @return data.frame, one variable in variable_of_interest as a row, with it's beta and p-value
#'
#' @export
logistic_wrapper = function(
    data,
    response,
    adjustments,
    by = NULL,
    variable_of_interest = NULL,
    variable_of_interest_formal_name = NULL,
    with_power = FALSE,
    variable_distribution = NULL){

  if (is.null(variable_of_interest_formal_name)){
    variable_of_interest_formal_name = variable_of_interest
  }

  if (is.null(by)){
    output = logistic_wrapper_(
      response = response,
      data = data,
      adjustments = adjustments,
      variable_of_interest = variable_of_interest,
      variable_of_interest_formal_name = variable_of_interest_formal_name,
      with_power = with_power,
      variable_distribution = variable_distribution
      )# %>%
      #generate_odd_ratio()
    return(output)
  }
  if (by %in% adjustments){
    adjustments_ = adjustments[!adjustments %in% by]
  }
  output = data %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(by))) %>%
    dplyr::group_modify(
      ~logistic_wrapper_(
        response = response,
        data = .data,
        adjustments = adjustments_,
        variable_of_interest = variable_of_interest,
        variable_of_interest_formal_name = variable_of_interest_formal_name,
        with_power = with_power,
        variable_distribution = variable_distribution) %>%
        tibble::rownames_to_column("variable"),
      .keep = TRUE
    )# %>%
    #generate_odd_ratio()
  return(output)
}
