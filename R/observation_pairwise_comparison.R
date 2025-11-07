utils::globalVariables(c("variable_name","group1", "group2","max_1", "max_2"))
#' Pairwise distribution comparison report for boolean(0/1) variable
#'
#' A cleaner wrapper running stats::pairwise.prop.test and clean the output
#'
#' @param data data.frame, the source data
#' @param variable str, the variable interested in
#' @param by str, the categorical variable stratifying the dataset
#' @param adjustment_method optional str, the correction adjusting pairwise, Benjamini-Hochberg FDR by default
#'
#' @return data.frame, the pairwise result for variable stratified by "by" in "data"
pairwise_boolean_test = function(data, variable, by, adjustment_method = "fdr"){
  result = data %>%
    dplyr::group_by(!!rlang::sym(by)) %>%
    dplyr::summarise(
      count = dplyr::n(),
      sum = !!rlang::sym(variable) %>% base::sum()) %>%
    tibble::column_to_rownames(var = by) %>%
    dplyr::do(
      stats::pairwise.prop.test(
        x = .data[,c("sum","count")] %>% base::as.matrix(),
        p.adjust.method = adjustment_method
      ) %>%
        broom::tidy()
    ) %>%
    dplyr::rename(
      p_value = "p.value"
    )
  return(result)
}

#' Pairwise distribution comparison report for boolean(0/1) variables
#'
#' A wrapper running allowing multiple variable of interest, will run the result in an interative manner
#'
#' @param data data.frame, the source data
#' @param variable str, the variable interested in
#' @param by str, the categorical variable stratifying the dataset
#' @param adjustment_method optional str, the correction adjusting pairwise, Benjamini-Hochberg FDR by default
#'
#' @return data.frame, the pairwise result for variable(s) stratified by "by" in "data"
#' @export
pairwise_boolean_table = function(data, variable, by, adjustment_method){
  result = data %>%
    dplyr::select(
      tidyr::all_of(c(by, variable))
    ) %>%
    tidyr::pivot_longer(
      -c(by),
      names_to = "variable_name",
      values_to = "value") %>%
    dplyr::group_by(variable_name) %>%
    dplyr::do(
      pairwise_boolean_test(
        data = .data,
        variable = "value",
        by = by,
        adjustment_method = adjustment_method)) %>%
    dplyr::ungroup()
  return(result)
}

#' A wrapper preparing pairwise distribution comparison report for boolean(0/1) variables
#'
#' This wrapper will run a stats::pairwise.prop.test for each element mentioned in "variable" while stratified across different value in "by" column and prepare a barchart for illustration.
#'
#' @param data data.frame, the source data
#' @param variable str or vector of str, the variable you are interested in
#' @param by str, the categorical variable stratifying the dataset
#' @param p_threshold optional float, the significance threshold, 0.05 by default
#' @param adjustment_method optional str, the correction adjusting pairwise, Benjamini-Hochberg FDR by default
#'
#' @return a list with following attributes:
#' * result: data.frame complete pairwise result.
#' * significant_result: data.frame which only include the significant result
#' * plot: ggplot2 instance, the figure illustrating the significant result.
#' @export
pairwise_boolean_report = function(
    data,
    variable,
    by,
    p_threshold = 0.05,
    adjustment_method = "fdr"){
  data = data %>%
    dplyr::select(
      tidyr::all_of(c(by, variable))
    )
  pairwise_result = pairwise_boolean_table(
    data = data,
    variable = variable,
    by = by,
    adjustment_method = adjustment_method
  )

  annotation_significant = pairwise_result %>%
    dplyr::filter(p_value < p_threshold) %>%
    dplyr::group_by(variable_name) %>%
    dplyr::mutate(
      p_value = p_value %>% round(2),
      y = 0.94 + 0.07*dplyr::row_number()) %>%
    dplyr::ungroup()

  for_figure = data %>%
    tidyr::pivot_longer(
      -c(by),
      names_to = "variable_name",
      values_to = "value") %>%
    dplyr::mutate(value = value %>% factor())

  g = ggplot2::ggplot(data = for_figure)+
    ggplot2::geom_bar(
      mapping = ggplot2::aes_string(
        x = by,
        fill = "value"),
      position = "fill")+
    ggsignif::geom_signif(
      data = annotation_significant,
      ggplot2::aes_string(
        xmin = "group1",
        xmax = "group2",
        annotations = "p_value",
        y_position="y"),
      textsize = 3,
      vjust = -0.2,
      manual = TRUE) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = c(0.2, 0.4, 0.6, 0.8, 1.0),
      expand = ggplot2::expansion(
        add = c(0,0.07)))+
    ggplot2::facet_wrap(
      facets = ggplot2::vars(variable_name),
      scales = "fixed")
  return(
    list("result" = pairwise_result,
         "significant_result" = annotation_significant %>% dplyr::select(-tidyr::all_of(c("y"))),
         "plot"= g))
}

#' Pairwise distribution comparison report for normally distributed variables
#'
#' A wrapper allowing multiple variable of interest, will run stats::pairwise.t.test in an iterative manner
#'
#' @param data data.frame, the source data
#' @param variable str, the variable interested in
#' @param by str, the categorical variable stratifying the dataset
#' @param adjustment_method optional str, the correction adjusting pairwise, Benjamini-Hochberg FDR by default
#'
#' @return data.frame, the pairwise result for variable(s) stratified by "by" in "data"
#' @export
pairwise_t_test_table = function(
    data,
    variable,
    by,
    adjustment_method){
  result = data %>%
    dplyr::select(
      tidyr::all_of(c(by, variable))
    ) %>%
    tidyr::pivot_longer(
      -c(by),
      names_to = "variable_name",
      values_to = "value") %>%
    dplyr::group_by(variable_name) %>%
    dplyr::do(
      stats::pairwise.t.test(
        .data %>% dplyr::pull(value),
        .data %>% dplyr::pull(by),
        p.adjust.method = adjustment_method,
        paired = FALSE,
        exact=FALSE) %>%
        broom::tidy()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(p_value = "p.value")
  return(result)
}

#' A wrapper preparing pairwise distribution comparison report normally distributed variables
#'
#' This wrapper will run a stats::pairwise.t.test for each element mentioned in "variable" while stratified across different value in "by" column and prepare a barchart for illustration.
#'
#' @param data data.frame, the source data
#' @param variable str or vector of str, the variable you are interested in
#' @param by str, the categorical variable stratifying the dataset
#' @param p_threshold optional float, the significance threshold, 0.05 by default
#' @param adjustment_method optional str, the correction adjusting pairwise, Benjamini-Hochberg FDR by default
#'
#' @return a list with following attributes:
#' * result: data.frame complete pairwise result.
#' * significant_result: data.frame which only include the significant result
#' * plot: ggplot2 instance, the figure illustrating the significant result.
#' @export
pairwise_t_test_report = function(
    data,
    variable,
    by,
    p_threshold = 0.05,
    adjustment_method = "fdr"){
  data = data %>%
    dplyr::select(
      tidyr::all_of(c(by, variable))
    )
  pairwise_result = pairwise_t_test_table(
    data = data,
    variable = variable,
    by = by,
    adjustment_method = adjustment_method
  )

  for_figure = data %>%
    tidyr::pivot_longer(
      -c(by),
      names_to = "variable_name",
      values_to = "value") %>%
    dplyr::group_by(!!rlang::sym(by), variable_name) %>%
    dplyr::reframe(
      mean = base::mean(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      max = mean + sd) %>%
    dplyr::ungroup()

  annotation_significant = pairwise_result %>%
    dplyr::filter(p_value < p_threshold) %>%
    dplyr::left_join(
      y = for_figure %>%
        dplyr::select(tidyr::all_of(c(by,"variable_name","max"))),
      by = dplyr::join_by(variable_name == variable_name, group1 == !!rlang::sym(by))
    ) %>%
    dplyr::left_join(
      y = for_figure %>%
        dplyr::select(tidyr::all_of(c(by,"variable_name","max"))),
      by = dplyr::join_by(variable_name == variable_name, group2 == !!rlang::sym(by)),
      suffix = c("_1", "_2")
    ) %>%
    dplyr::mutate(
      max_annotation = base::pmax(max_1, max_2),
      p_value = p_value %>% round(2)) %>%
    dplyr::group_by(variable_name) %>%
    dplyr::mutate(
      max_annotation = (.data$max_annotation %>% max()) * (0.85+0.2*dplyr::row_number())
    ) %>%
    dplyr::ungroup()

  g = ggplot2::ggplot(data = for_figure)+
    ggplot2::geom_bar(
      mapping = ggplot2::aes_string(
        x = by,
        y = "mean"
      ),
      stat = "identity")+
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        x = !!rlang::sym(by),
        ymin = mean-sd,
        ymax = mean+sd),
      width=.2,
    )+
    ggsignif::geom_signif(
      data = annotation_significant,
      ggplot2::aes_string(
        xmin = "group1",
        xmax = "group2",
        annotations = "p_value",
        y_position="max_annotation"),
      textsize = 3,
      vjust = -0.2,
      manual = TRUE) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(variable_name),
      scales = "free_y")
  return(
    list("result" = pairwise_result,
         "significant_result" = annotation_significant %>%
           dplyr::select(
             tidyr::all_of(
               c("variable_name","group1","group2","p_value"))),
         "plot"= g))
}

#' Pairwise distribution comparison report for non-normally distributed variables
#'
#' A wrapper allowing multiple variable of interest, will run stats::pairwise.wilcox.test in an iterative manner
#'
#' @param data data.frame, the source data
#' @param variable str, the variable interested in
#' @param by str, the categorical variable stratifying the dataset
#' @param adjustment_method optional str, the correction adjusting pairwise, Benjamini-Hochberg FDR by default
#'
#' @return data.frame, the pairwise result for variable(s) stratified by "by" in "data"
#' @export
pairwise_wilcox_table = function(
    data,
    variable,
    by,
    adjustment_method){
  result = data %>%
    dplyr::select(
      tidyr::all_of(c(by, variable))
    ) %>%
    tidyr::pivot_longer(
      -c(by),
      names_to = "variable_name",
      values_to = "value") %>%
    dplyr::group_by(variable_name) %>%
    dplyr::do(
      stats::pairwise.wilcox.test(
        .data %>% dplyr::pull(value),
        .data %>% dplyr::pull(by),
        p.adjust.method = adjustment_method,
        paired = FALSE,
        exact=FALSE) %>%
        broom::tidy()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(p_value = "p.value")
  return(result)
}

#' A wrapper preparing pairwise distribution comparison report normally distributed variables
#'
#' This wrapper will run a stats::pairwise.t.test for each element mentioned in "variable" while stratified across different value in "by" column and prepare a barchart for illustration.
#'
#' @param data data.frame, the source data
#' @param variable str or vector of str, the variable you are interested in
#' @param by str, the categorical variable stratifying the dataset
#' @param p_threshold optional float, the significance threshold, 0.05 by default
#' @param adjustment_method optional str, the correction adjusting pairwise, Benjamini-Hochberg FDR by default
#'
#' @return a list with following attributes:
#' * result: data.frame complete pairwise result.
#' * significant_result: data.frame which only include the significant result
#' * plot: ggplot2 instance, the figure illustrating the significant result.
#' @export
pairwise_wilcox_report = function(
    data,
    variable,
    by,
    p_threshold = 0.05,
    adjustment_method = "fdr"){
  data = data %>%
    dplyr::select(
      tidyr::all_of(c(by, variable))
    )
  pairwise_result = pairwise_wilcox_table(
    data = data,
    variable = variable,
    by = by,
    adjustment_method = adjustment_method
  )

  for_figure = data %>%
    tidyr::pivot_longer(
      -c(by),
      names_to = "variable_name",
      values_to = "value")

  max = for_figure %>%
    dplyr::group_by(!!rlang::sym(by), variable_name) %>%
    dplyr::reframe(max = base::max(value, na.rm = TRUE))

  annotation_significant = pairwise_result %>%
    dplyr::filter(p_value < p_threshold) %>%
    dplyr::left_join(
      y = max,
      by = dplyr::join_by(variable_name == variable_name, group1 == !!rlang::sym(by))
    ) %>%
    dplyr::left_join(
      y = max,
      by = dplyr::join_by(variable_name == variable_name, group2 == !!rlang::sym(by)),
      suffix = c("_1", "_2")
    ) %>%
    dplyr::mutate(
      max_annotation = base::pmax(max_1, max_2),
      p_value = p_value %>% round(2)) %>%
    dplyr::group_by(variable_name) %>%
    dplyr::mutate(
      max_annotation = (.data$max_annotation %>% base::max(na.rm = TRUE)) * (0.75+0.25*dplyr::row_number())
    ) %>%
    dplyr::ungroup()

  g = ggplot2::ggplot(data = for_figure)+
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes_string(
        x = by,
        y = "value"
      ),
      width = 0.3)+
    ggsignif::geom_signif(
      data = annotation_significant,
      ggplot2::aes_string(
        xmin = "group1",
        xmax = "group2",
        annotations = "p_value",
        y_position="max_annotation"),
      textsize = 3,
      vjust = -0.2,
      manual = TRUE) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(variable_name),
      scales = "free_y")+
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(
        mult = c(0.1,0.3)))
  return(
    list("result" = pairwise_result,
         "significant_result" = annotation_significant %>%
           dplyr::select(
             tidyr::all_of(
               c("variable_name","group1","group2","p_value"))),
         "plot"= g))
}
