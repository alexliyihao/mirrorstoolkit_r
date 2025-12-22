utils::globalVariables(c("variable", "p_value", "normality"))
#' Shapiro-Wilk test on each variable
#'
#' For a data.frame, run Shapiro-Wilk test on each column, if a "by" is specified, it will run a stratified Shapiro-wilk test by this column
#' @importFrom rlang .data
#'
#' @param data, data.frame, the data table to be tested
#' @param by, optional str, the column by which data will be stratified
#'
#' @return data.frame, a data frame including all the results
#' @export
normality_table = function(data, by = NULL){
  normality_general = data %>%
    data.table::copy() %>%
    dplyr::select(-tidyr::all_of(by)) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "variable",
      values_to = "value") %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      p_value = stats::shapiro.test(.data[["value"]])$p.value
    ) %>%
    dplyr::mutate(normality = p_value > 0.05)

  if (!is.null(by)){
    normality_stratified = data %>%
      data.table::copy() %>%
      tidyr::pivot_longer(
        -c(!!rlang::sym(by)),
        names_to = "variable",
        values_to = "value") %>%
      dplyr::group_by(!!rlang::sym(by), variable) %>%
      dplyr::summarise(
        p_value = stats::shapiro.test(.data[["value"]])$p.value
      ) %>%
      dplyr::mutate(
        normality = p_value > 0.05,
        output = paste(normality, "(", sprintf("%.1e", p_value) , ")", sep = "")) %>%
      dplyr::select(tidyr::all_of(c(by, "variable", "output"))) %>%
      tidyr::pivot_wider(names_from = by, values_from = "output")
    normality_general = base::merge(
      x = normality_general,
      y = normality_stratified,
    )
  }
  return(normality_general)
}


#' Wrapper quickly check the distribution from Shapiro-Wilk test and visualization
#'
#' @param data, data.frame, the data table with original data
#' @param variable, str or vector of str, the column name of variable of interest
#' @param by, optional str, if given, the column by which data will be stratified
#' @param palette_color, optional vector of str, list of colors used
#'
#' @return a list with two element, "visualization" is a list of ggplot instance with the histogram and density plot, "shapiro-wilk_test" is a data.frame
#' @export
inspect_distribution = function(
    data,
    variable,
    by = NULL,
    palette_color = NULL){
  if (!is.vector(variable)){
    variable = c(variable)
  }
  df = data %>%
    dplyr::select(tidyr::all_of(c(variable, by))) %>%
    data.table::copy()
  if (!is.null(by)){
    df = df %>% dplyr::mutate(!!rlang::sym(by) := !!rlang::sym(by) %>% factor())
  }
  plot_list = list()
  for (i in variable){
    if (is.null(by)){
      mapping_fill = ggplot2::aes(x = !!rlang::sym(i))
      mapping_color = ggplot2::aes(x = !!rlang::sym(i))
      palette_color = distinguishable_palette(
        num_color = 1,
        pre_specified_palette = palette_color
      )
    } else {
      mapping_fill = ggplot2::aes(
        x = !!rlang::sym(i),
        fill = !!rlang::sym(by))
      mapping_color = ggplot2::aes(
        x = !!rlang::sym(i),
        color = !!rlang::sym(by))
      palette_color = distinguishable_palette(
        num_color = df %>%
          dplyr::pull(!!rlang::sym(by)) %>%
          dplyr::n_distinct(),
        pre_specified_palette = palette_color
      )
    }
    histogram_plot = ggplot2::ggplot(data = df)+
      ggplot2::geom_histogram(
        mapping = mapping_fill,
        bins = 7,
        position = ggplot2::position_dodge()
      )+
      ggplot2::scale_fill_manual(
        values = palette_color
      )+
      ggplot2::theme(aspect.ratio = 1)
    plot_list = plot_list %>% append(histogram_plot)
    density_plot = ggplot2::ggplot(data = df)+
      ggplot2::geom_density(
        mapping = mapping_color
      )+
      ggplot2::scale_color_manual(
        values = palette_color
      )+
      ggplot2::theme(aspect.ratio = 1)
    plot_list = plot_list %>% append(density_plot)
  }
  df_shapiro = normality_table(
    data = df,
    by = by)
  aggregate = ggpubr::ggarrange(
    plotlist = plot_list,
    ncol = 2,
    common.legend = TRUE)
  return(list(
    "visualization" = aggregate,
    "shapiro-wilk_test" = df_shapiro %>% as.data.frame()))
}
