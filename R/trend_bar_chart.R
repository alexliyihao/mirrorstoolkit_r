utils::globalVariables(c("Proportion"))
#' A fill bar chart for composition over a specific variable
#'
#' This function returns a filled bar chart illustrate the trend of a composition over the change of a specific variable
#'
#' @param data, data.frame, The table working on
#' @param id, str, the name of column of subject id
#' @param by, str, the name of column of variable on x
#' @param variable, optional vector of str, the name of column of composition components if specified, otherwise will use all the other variables
#' @param legend_title, str, the legend title in the figure
#' @param palette_color, list or str, pre-specfied colors used for different phenotypes
#'
#' @return ggplot2 instance, the plot output
#' @export
trend_bar_chart = function(
    data,
    id,
    by,
    variable = NULL,
    legend_title = "Component",
    palette_color = NULL){
  if (base::is.null(variable)){
    for_figure = data %>%
      data.table::copy()
  } else {
    for_figure = data %>%
      data.table::copy() %>%
      dplyr::select(
        tidyr::all_of(c(id, by, variable))
      )
  }
  for_figure = for_figure %>%
    dplyr::mutate(
      !!rlang::sym(id) := !!rlang::sym(id) %>%
        forcats::fct_reorder(
          !!rlang::sym(by),
          .desc = FALSE
          )
      ) %>%
    tidyr::pivot_longer(
      -c(!!rlang::sym(id), !!rlang::sym(by)),
      names_to = legend_title,
      values_to = "Proportion"
      )

  palette_color = distinguishable_palette(
    num_color = for_figure %>%
      dplyr::pull(!!rlang::sym(legend_title)) %>%
      dplyr::n_distinct(),
    pre_specified_palette = palette_color
  )

  g = ggplot2::ggplot(data = for_figure)+
    ggplot2::geom_bar(
      ggplot2::aes(x = !!rlang::sym(id),
          y = Proportion,
          fill = !!rlang::sym(legend_title)),
      position = "fill",
      stat = "identity") +
    ggplot2::scale_fill_manual(
      values = palette_color) +
    ggplot2::scale_y_continuous(
      name = "percentage",
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0)))+
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90))+
    ggplot2::guides(
      fill=ggplot2::guide_legend(title=legend_title))
  return(g)
}
