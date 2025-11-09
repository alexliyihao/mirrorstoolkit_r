utils::globalVariables(c("count", "median","q1", "q3", "sd"))
#' Characteristics for boolean variable
#'
#' A function extract boolean 1/total(percentage) format from a column named variable from table df, when by has and only has 2 classes, run a chi-square test
#'
#' @param variable str, The name of column in table df
#' @param df data.frame, the table working on
#' @param by str, stratification variable name
#'
#' @return data.frame, frequency and relative frequency for the second term in table() and prop.table which is designed to be 1 (positive indicator for a disease against as negative)
generate_boolean_ratio_output = function(variable, df, by){
  # Compute the 1/all ratio for each stratified group
  output = df %>% dplyr::select(
    tidyr::all_of(c(by, variable)))%>%
    dplyr::group_by(!!rlang::sym(by)) %>%
    dplyr::summarize(
      ones = sum(!!rlang::sym(variable) == 1, na.rm = TRUE),
      count = sum(!is.na(!!rlang::sym(variable))),
      output = paste(
        c(ones,
         "/",
         count,
         "(",
         (ones/count * 100) %>% round(2),
         "%)"),
       collapse = "", sep = "") ) %>%
    dplyr::select(tidyr::all_of(c(by, "output")))
  # add a separate overall column, which is often required
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
  # If the "by" column gives more than 1 class, run a chisq.test
  if (df %>% dplyr::select(by) %>% dplyr::n_distinct() >= 2){
    subtable = df %>% dplyr::select(tidyr::all_of(c(by, variable))) %>% table()
    chisquare_p = stats::chisq.test(subtable,simulate.p.value = TRUE)$p.value
    output = output %>% tibble::add_row(
      !!by := "p_value",
      !!variable := sprintf("%.4e", chisquare_p))
  }
  return(output)
}

#' Characteristics for non-normality distributed variable
#'
#' Extract median(IQR) format from a column named variable from table df,
#' when "by" has and only has 2 classes, run a Mann-Whitney U test
#'
#' @param variable str, The name of column in table df
#' @param df data.frame, the table working on
#' @param by str, stratification variable name
#'
#' @return data.frame, the characteristic data median(IQR), when by has and only has 2 classes, run a Mann Whitney U test
generate_continuous_IQR_output = function(variable, df, by){
  output = df %>% dplyr::select(
    tidyr::all_of(c(by, variable)))%>%
    dplyr::group_by(!!rlang::sym(by)) %>%
    dplyr::summarize(
      q1 = stats::quantile(!!rlang::sym(variable), 0.25, na.rm = TRUE),
      median = !!rlang::sym(variable) %>% stats::median(na.rm = TRUE),
      q3 = stats::quantile(!!rlang::sym(variable), 0.75,na.rm = TRUE),
      output = paste(c(median %>% round(2),
                       "(", q1 %>% round(2),"-",q3 %>% round(2), ")"),
                     collapse = "", sep = "") ) %>%
    dplyr::select(tidyr::all_of(c(by, "output")))
  # add a separate overall column, which is often required
  overall = df %>% dplyr::pull(variable)
  output = output %>% tibble::add_row(
    !!by := "overall",
    output = paste(
      c(overall %>% stats::median(na.rm = TRUE) %>% round(2),
        "(",
        overall %>% stats::quantile(0.25, na.rm = TRUE) %>% round(2),
        "-",
        overall %>% stats::quantile(0.75, na.rm = TRUE) %>% round(2), ")"),
      collapse = "", sep = ""),
    .before = 1
  ) %>% magrittr::set_colnames(
    c(by,
      variable))

  n_by = df %>% dplyr::select(by) %>% dplyr::n_distinct()
  if (n_by > 1){
    subtable = df %>%
      dplyr::select(tidyr::all_of(c(by, variable))) %>%
      dplyr::mutate(!!rlang::sym(by) := !!rlang::sym(by) %>% base::factor())
    if (n_by == 2){# If the "by" column only gives 2 class, run a wilcox.test (Mann-Whittney U test)
      p = stats::wilcox.test(
        base::paste("`",variable,"`", "~", "`",by,"`", collapse = "", sep = "") %>% stats::as.formula(),
        data = subtable,
        exact = FALSE)$p.value
    } else {# If the "by" column have more than 2 class, run a kruskal.test (Kruskal-Wallis rank sum test)
      p = stats::kruskal.test(
        x = subtable %>% dplyr::pull(variable),
        g = subtable %>% dplyr::pull(by),
        exact = FALSE)$p.value
    }
    output = output %>% tibble::add_row(
      !!by := "p_value",
      !!variable := sprintf("%.4e", p))
  }
  return(output)
}

#' Characteristics for normality distributed variable
#'
#' A function extract mean(sd) format from a column named variable from table df,
#' when "by" has and only has 2 classes, run a Mann Whitney U test or two sample t-test
#' user can specify normal_policy = "wilcox" or "t-test"
#'
#' @param variable str, The name of column in table df
#' @param df data.frame, the table working on
#' @param by str, stratification variable name
#' @param normal_policy str, default "wilcox",for it has relaxed assumptions
#' * if "wilcox", run stats::wilcox.test(2 categories) or stats::kruskal.test(3+)
#' * if "t-test", run stats::t.test(2 categories), or stats::aov(3+)
#'
#' @return data.frame, the characteristic data mean(sd), when by has and only has 2 classes, run a Mann Whitney U test
#' @export
generate_continuous_sd_output = function(variable, df, by, normal_policy = "wilcox"){
  output = df %>% dplyr::select(
    tidyr::all_of(c(by, variable)))%>%
    dplyr::group_by(!!rlang::sym(by)) %>%
    dplyr::summarize(
      mean = mean(!!rlang::sym(variable), na.rm = TRUE),
      sd = stats::sd(!!rlang::sym(variable), na.rm = TRUE),
      output = paste(c(mean %>% round(2),
                       " \u00B1 ", sd %>% round(2)),
                     collapse = "", sep = "")
    ) %>%
    dplyr::select(tidyr::all_of(c(by, "output")))
  overall = df %>% dplyr::pull(variable)
  output = output %>% tibble::add_row(
    !!by := "overall",
    output = paste(
      c(overall %>% mean(na.rm = TRUE) %>% round(2),
        " \u00B1 ", overall %>% stats::sd(na.rm = TRUE) %>% round(2)),
      collapse = "", sep = ""),
    .before = 1
  ) %>% magrittr::set_colnames(
    c(by,
      variable))
  # If the "by" column only gives 2 class, run a wilcox.test (Mann-Whittney U test)
  # or two-sample t.test without same sample size assumptions (Welch's t-test)
  # most of the times my data cannot support the sample size or perfect normality
  # thus wilcox.test is default, t-test option is given here

  n_by = df %>% dplyr::select(by) %>% dplyr::n_distinct()
  if (n_by > 1){
    subtable = df %>%
      dplyr::select(tidyr::all_of(c(by, variable))) %>%
      dplyr::mutate(!!rlang::sym(by) := !!rlang::sym(by) %>% base::factor())
    if (n_by == 2){# If the "by" column only gives 2 class, run a wilcox.test (Mann-Whittney U test)
      if (normal_policy == "wilcox"){
        p = stats::wilcox.test(
          base::paste("`",variable,"`", "~", "`",by,"`", collapse = "", sep = "") %>% stats::as.formula(),
          data = subtable,
          exact = FALSE)$p.value
        } else if (normal_policy == "t-test") {
          p = stats::t.test(
            paste("`",variable,"`", "~", "`",by,"`", collapse = "", sep = "") %>% stats::as.formula(),
            data = subtable,
            exact = FALSE,
            var.equal = FALSE)$p.value
        }
      } else {# If the "by" column have more than 2 class, run a kruskal.test (Kruskal-Wallis rank sum test)
        if (normal_policy == "wilcox"){
          p = stats::kruskal.test(
            x = subtable %>% dplyr::pull(variable),
            g = subtable %>% dplyr::pull(by),
            exact = FALSE)$p.value
        } else if (normal_policy == "t-test"){
          aov_fit = stats::aov(
            base::paste("`",variable,"`", "~", "`",by,"`", collapse = "", sep = "") %>% stats::as.formula(),
            data = subtable)
          p = summary(aov_fit)[[1]][1, 5]
        }
      }
    output = output %>% tibble::add_row(
      !!by := "p_value",
      !!variable := sprintf("%.4e", p))
    }
    return(output)
}

#' Single column inspector
#'
#' Wrapper function cleaning the format of generated statistic
#'
#' @param data data.frame, the data.frame working on
#' @param variable_list character vector, the vector of variables included
#' @param by character, the column name used for stratifying
#' @param mode character, the mode generating result, can be "boolean", "continuous_sd", "continuous_IQR"
#' @param normal_policy str, for "continuous_sd" mode, if "wilcox", run stats::wilcox.test, if "t-test", run stats::t.test
#'
#' @return data.frame, the cleaned result
characteristic_wrapper = function(
    data,
    variable_list,
    by,
    mode = "boolean",
    normal_policy = "wilcox"){
  result_lists = list()
  data = data %>% dplyr::mutate(
    !!rlang::sym(by):= !!rlang::sym(by) %>% as.character())
  if (mode == "boolean"){
    for (i in variable_list){
      result_lists[[i]] = generate_boolean_ratio_output(
        variable = i,
        df = data,
        by = by)
      }
    } else if (mode == "continuous_sd"){
      for (i in variable_list){
        result_lists[[i]] = generate_continuous_sd_output(
          variable = i,
          df = data,
          by = by,
          normal_policy = normal_policy)
        }
    } else if (mode == "continuous_IQR"){
      for (i in variable_list){
        result_lists[[i]] = generate_continuous_IQR_output(
          variable = i,
          df = data,
          by = by)
      }
    }
  output = result_lists %>%
    purrr::reduce(dplyr::full_join, by = by) %>%
    tibble::column_to_rownames(by) %>%
    t() %>%
    as.data.frame()
  return(output)
}

#' Characteristic Table Generator
#'
#' Wrapper function cleaning the format of generated statistic
#'
#' @param data data.frame, the data.frame working on
#' @param by character, the column name used for stratifying
#' @param boolean_list optional character vector, the column name which is treated as boolean variable
#' @param sd_list optional character vector, the column name which is treated as normal distributed continuous variable
#' @param IQR_list optional character vector, the column name which is treated as non-normal distributed continuous variable
#' @param order optional character vector, if specified, the rows will follow the order provided
#' @param normal_policy str, for variables in sd_list, if "wilcox", run stats::wilcox.test, if "t-test", run stats::t.test
#'
#' @return data.frame, the cleaned result
#' @export
characteristic_table_generator = function(
    data,
    by=NULL,
    boolean_list=NULL,
    sd_list=NULL,
    IQR_list=NULL,
    order=NULL,
    normal_policy="wilcox"
){
  if (base::is.null(by)){
    by = "characteristic_table_overall"
    data = data %>% dplyr::mutate(
      !!rlang::sym(by):= "_")
  } else {
    data = data %>%
      dplyr::mutate(!!rlang::sym(by):= !!rlang::sym(by) %>% as.character())
  }
  if (!is.null(boolean_list)){
    boolean_result = characteristic_wrapper(
      data = data,
      variable_list = boolean_list,
      by = by,
      mode = "boolean")
  } else {
    boolean_result = NULL
  }
  if (!is.null(sd_list)){
    sd_result = characteristic_wrapper(
      data = data,
      variable_list = sd_list,
      by = by,
      mode = "continuous_sd",
      normal_policy = normal_policy)
  } else {
    sd_result = NULL
  }
  if (!is.null(IQR_list)){
    IQR_result = characteristic_wrapper(
      data = data,
      variable_list = IQR_list,
      by = by,
      mode = "continuous_IQR")
  } else {
    IQR_result = NULL
  }
  characteristic_table = rbind(boolean_result, sd_result, IQR_result)
  if (!is.null(order)){
    characteristic_table = characteristic_table[order,]
  }
  return(characteristic_table %>% dplyr::select(-tidyr::any_of(c("_"))))
}


#' Multi-column Summary
#'
#' Wrapper function output multiple column base::summary() result into data.frame
#' I really cannot understand why R returns that in that weird way.
#'
#' @param data data.frame, the data.frame working on
#'
#' @return data.frame, the cleaned result
#' @export
multi_line_summary = function(data){
  base::do.call(
    base::cbind,
    base::lapply(data, base::summary)) %>%
    base::t() %>%
    base::as.data.frame()
}
