#' Characteristics for boolean variable
#'
#' A function extract boolean 1/total(percentage) format from a column named variable from table df, when by has and only has 2 classes, run a chi-square test
#'
#' @param variable The name of column in table df
#' @param df the table working on
#' @param by str, stratification variable name
#' @return data.frame, frequency and relative frequency for the second term in table() and prop.table which is designed to be 1 (positive indicator for a disease against as negative)
generate_boolean_ratio_output = function(variable, df, by){
  output = df %>% dplyr::select(
    tidyr::all_of(c(by, variable)))%>%
    dplyr::group_by(!!rlang::sym(by)) %>%
    summarize(
      ones = sum(!!rlang::sym(variable) == 1, na.rm = TRUE),
      count = sum(!is.na(!!rlang::sym(variable))),
      output = paste(c(ones %>% round(2),
                       "/",
                       count %>% round(2),
                       "(",(ones/count * 100) %>% round(2),
                       "%)"),
                     collapse = "", sep = "") ) %>%
    dplyr::select(tidyr::all_of(c(by, "output")))
  overall = df %>% dplyr::pull(variable)
  ones = sum(overall == 1)
  all = sum(!is.na(overall))
  output = output %>% tibble::add_row(
    !!by := "overall",
    output = paste(
      c(
        ones %>% round(2),
        "/",
        all %>% round(2),
        "(",
        (ones/all * 100) %>% round(2),
        "%)"),
      collapse = "",
      sep = ""),
    .before = 1
  ) %>% magrittr::set_colnames(
    c(by,
      variable))

  if (df %>% dplyr::select(by) %>% dplyr::n_distinct() == 2){
    subtable = df %>% dplyr::select(tidyr::all_of(c(by, variable))) %>% table()
    chisquare_p = stats::chisq.test(subtable,simulate.p.value = TRUE)$p.value
    output = output %>% tibble::add_row(
      !!by := "p-value",
      !!variable := sprintf("%.4e", chisquare_p))
  }
  return(output)
}

#' Characteristics for non-normality distributed variable
#'
#' a function extract median(IQR) format from a column named variable from table df, when by has and only has 2 classes, run a Mann Whitney U test
#'
#' @param variable str, The name of column in table df
#' @param df the table working on
#' @param by str, stratification variable name
#' @return data.frame, the characteristic data median(IQR), when by has and only has 2 classes, run a Mann Whitney U test
generate_continuous_IQR_output = function(variable, df, by){
  output = df %>% dplyr::select(
    tidyr::all_of(c(by, variable)))%>%
    dplyr::group_by(!!rlang::sym(by)) %>%
    summarize(
      q1 = quantile(!!rlang::sym(variable), 0.25, na.rm = TRUE),
      median = !!rlang::sym(variable) %>% median(na.rm = TRUE),
      q3 = quantile(!!rlang::sym(variable), 0.75,na.rm = TRUE),
      output = paste(c(median %>% round(2),
                       "(", q1 %>% round(2),"-",q3 %>% round(2), ")"),
                     collapse = "", sep = "") ) %>%
    dplyr::select(tidyr::all_of(c(by, "output")))
  overall = df %>% dplyr::pull(variable)
  output = output %>% tibble::add_row(
    !!by := "overall",
    output = paste(
      c(overall %>% median(na.rm = TRUE) %>% round(2),
        "(",
        overall %>% quantile(0.25, na.rm = TRUE) %>% round(2),
        "-",
        overall %>% quantile(0.75, na.rm = TRUE) %>% round(2), ")"),
      collapse = "", sep = ""),
    .before = 1
  ) %>% magrittr::set_colnames(
    c(by,
      variable))

  if (df %>% dplyr::select(by) %>% dplyr::n_distinct() == 2){
    subtable = df %>%
      dplyr::select(tidyr::all_of(c(by, variable))) %>%
      dplyr::mutate(by = by %>% as.factor())
    wilcox_p = stats::wilcox.test(
      paste("`",variable,"`", "~", "`",by,"`", collapse = "", sep = "") %>% as.formula(),
      data = subtable,
      exact = FALSE)$p.value
    output = output %>% tibble::add_row(
      !!by := "p-value",
      !!variable := sprintf("%.4e", wilcox_p))
  }
  return(output)
}

#' Characteristics for normality distributed variable
#'
#' A function extract mean(sd) format from a column named variable from table df, when by has and only has 2 classes, run a Mann Whitney U test (I didn't deploy 2 sample t-test for I cannot assume sample size and variance)
#'
#' @param variable The name of column in table df
#' @param df the table working on
#' @return data.frame, the characteristic data mean(sd), when by has and only has 2 classes, run a Mann Whitney U test
generate_continuous_sd_output = function(variable, df, by){
  output = df %>% dplyr::select(
    tidyr::all_of(c(by, variable)))%>%
    dplyr::group_by(!!rlang::sym(by)) %>%
    summarize(
      mean = mean(!!rlang::sym(variable), na.rm = TRUE),
      sd = sd(!!rlang::sym(variable), na.rm = TRUE),
      output = paste(c(mean %>% round(2),
                       " ± ", sd %>% round(2)),
                     collapse = "", sep = "")
    ) %>%
    dplyr::select(tidyr::all_of(c(by, "output")))
  overall = df %>% dplyr::pull(variable)
  output = output %>% tibble::add_row(
    !!by := "overall",
    output = paste(
      c(overall %>% mean(na.rm = TRUE) %>% round(2),
        " ± ", overall %>% sd(na.rm = TRUE) %>% round(2)),
      collapse = "", sep = ""),
    .before = 1
  ) %>% magrittr::set_colnames(
    c(by,
      variable))

  if (df %>% dplyr::select(by) %>% dplyr::n_distinct() == 2){
    subtable = df %>%
      dplyr::select(tidyr::all_of(c(by, variable))) %>%
      dplyr::mutate(by = by %>% as.factor())
    wilcox_p = stats::wilcox.test(
      paste("`",variable,"`", "~", "`",by,"`", collapse = "", sep = "") %>% as.formula(),
      data = subtable,
      exact = FALSE)$p.value
    output = output %>% tibble::add_row(
      !!by := "p-value",
      !!variable := sprintf("%.4e", wilcox_p))
  }
  return(output)
}

#' Single column inspector
#'
#' a wrapper function cleaning the format from generated statistic
#'
#' @param data data.frame, the data.frame working on
#' @param variable_list character vector, the vector of variables included
#' @param by character, the column name used for stratifying
#' @param mode character, the mode generating result, can be "boolean", "continuous_sd', "continuous_IQR"
#' @return data.frame, the cleaned result
#' @export
characteristic_wrapper = function(data, variable_list, by, mode = "boolean"){
  result_lists = list()
  for (i in variable_list){
    if (mode == "boolean"){
      result_lists[[i]] = generate_boolean_ratio_output(
        variable = i,
        df = data,
        by = by)
    } else if (mode == "continuous_sd"){
      result_lists[[i]] = generate_continuous_sd_output(
        variable = i,
        df = data,
        by = by)
    } else if (mode == "continuous_IQR"){
      result_lists[[i]] = generate_continuous_IQR_output(
        variable = i,
        df = data,
        by = by)
    } }
  output = result_lists %>%
    purrr::reduce(dplyr::full_join, by = by) %>%
    tibble::column_to_rownames(by) %>%
    t() %>%
    as.data.frame()
  return(output)
}

#' Characteristic Table Generator
#'
#' A wrapper function cleaning the format from generated statistic
#'
#' @param data data.frame, the data.frame working on
#' @param by character, the column name used for stratifying
#' @param boolean_list character vector, the column name which is treated as boolean variable
#' @param sd_list character vector, the column name which is treated as normal distributed continuous variable
#' @param IQR_list character vector, the column name which is treated as non-normal distributed continuous variable
#' @param order optional character vector, if specified, the row will follow the order provided
#' @return data.frame, the cleaned result
#' @export
characteristic_table_generator = function(
    data, by, boolean_list, sd_list, IQR_list, order=NULL
){
  boolean = characteristic_wrapper(
    data = data,
    variable_list = boolean_list,
    by = by,
    mode = "boolean")
  sd = characteristic_wrapper(
    data = data,
    variable_list = sd_list,
    by = by,
    mode = "continuous_sd")
  IQR = characteristic_wrapper(
    data = data,
    variable_list = IQR_list,
    by = by,
    mode = "continuous_IQR")
  characteristic_table = rbind(boolean, sd, IQR)
  if (is.null(order)){
    return(characteristic_table)
  } else(
    return(characteristic_table[order,])
  )
}
