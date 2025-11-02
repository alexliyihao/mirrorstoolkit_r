#' Clean format for a Manhattan plot
#'
#' A function cleans column format for a Manhattan plot, in which "phenotype" and "coding" should be factor, "pos" and "p-value" should be numerical, formal name should be character
#'
#' @param data, data.frame, The table working on
#' @param pos, str, the name of column of position of the variant
#' @param phenotype, str, the name of column of phenotype the variant associated to
#' @param p_value, str, the name of column of p-values
#' @param formal_name, optional str, the name of column of formal name of each variant
#' @param coding, optional str, (currently not used) the name of column of exonic vs intronic,
#' @param phenotype_list, optional vector of str, the list of phenotypes which is put into consideration
#'
#' @return data.frame, the table cleaned
manhattan_data_clean = function(
    data,
    pos,
    phenotype,
    p_value,
    formal_name = NULL,
    coding = NULL,
    phenotype_list = NULL){
  for_figure = data.table::copy(data)
  if (!is.null(phenotype_list)){
    for_figure = for_figure %>%
      dplyr::filter(
        (!!rlang::sym(phenotype)) %in% phenotype_list
      ) %>%
      dplyr::mutate(
        (!!rlang::sym(phenotype)) := (!!rlang::sym(phenotype)) %>%
          factor(levels = phenotype_list)
      )
  } else {
    for_figure = for_figure %>%
      dplyr::mutate(
        (!!rlang::sym(phenotype)) := (!!rlang::sym(phenotype)) %>% factor()
      )
  }
  for_figure = for_figure %>% dplyr::mutate(
    (!!rlang::sym(pos)) := (!!rlang::sym(pos)) %>% as.numeric(),
    (!!rlang::sym(p_value)) := (!!rlang::sym(p_value)) %>% as.numeric(),
  )
  if (!is.null(coding)){
    for_figure = for_figure %>% dplyr::mutate(
      (!!rlang::sym(coding)) := (!!rlang::sym(coding)) %>% factor()
    )
  }
  if (!is.null(formal_name)){
    for_figure = for_figure %>% dplyr::mutate(
      (!!rlang::sym(formal_name)) := (!!rlang::sym(formal_name)) %>% as.character()
    )
  }
  return(for_figure)
}

#' Wrapper for a Manhattan plot
#'
#' A wrapper function cleans column format for a Manhattan plot
#'
#' @param data, data.frame, The table working on
#' @param pos, string, the name of column of position of the variant
#' @param phenotype, str, the name of column of phenotype the variant associated to
#' @param p_value, str, the name of column of p-values
#' @param p_value_threshold, float, the threshold for significance, default 0.05
#' @param formal_name, optional str, the name of column of formal name of each variant, if not specified, will use pos
#' @param coding, optional str, (currently not used), the name of column of exonic vs intronic,
#' @param phenotype_list, optional vector of str, the list of phenotypes which is put into consideration
#' @param highlight_region, optional list of vector of str, in list(c(start, end)...) format, the list of highlighted region
#' @param palette_color, list or str, pre-specfied colors used for different phenotypes
#' @param coord_ratio, float, the coordinate ratio for the figure
#'
#' @return ggplot2 instance, the Manhattan plot
#' @export
manhattan_plot = function(
    data,
    pos,
    phenotype,
    p_value,
    p_value_threshold = 0.05,
    formal_name = NULL,
    coding = NULL,
    phenotype_list = NULL,
    highlight_region = NULL,
    palette_color = NULL,
    coord_ratio = 2500/1){

  for_figure = manhattan_data_clean(
    data = data,
    pos = pos,
    phenotype = phenotype,
    p_value = p_value,
    formal_name = formal_name,
    coding = coding,
    phenotype_list = phenotype_list)

    palette_color = distinguishable_palette(
      num_color = for_figure %>%
        dplyr::pull(!!rlang::sym(phenotype)) %>%
        dplyr::n_distinct(),
      pre_specified_palette = palette_color
      )

  significant_snps = for_figure %>%
    dplyr::filter((!!rlang::sym(p_value)) < p_value_threshold)

  g = ggplot2::ggplot(for_figure)

  if (!is.null(highlight_region)){
    for (i in highlight_region){
      g = g + ggplot2::geom_rect(
        color = "lightgrey",
        fill = "lightgrey",
        linewidth = 0.01,
        xmin = i[1],
        xmax = i[2],
        ymin = 0,
        ymax = Inf,
        alpha = 0.01)
    }
  }
  g = g + ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = pos,
      y = -log10((!!rlang::sym(p_value))),
      color = phenotype
    ),
    alpha=1,
    size=1,
    stroke = FALSE
    )+
    ggplot2::geom_hline(
      yintercept = -log10(p_value_threshold),
      color = "red",
      alpha = 0.5)+
    ggplot2::scale_color_manual(
      values = palette_color)+
    ggnewscale::new_scale_color()+
    ggnewscale::new_scale_fill()+
    ggrepel::geom_label_repel(
      data = significant_snps,
      mapping = ggplot2::aes(
        label=(!!rlang::sym(formal_name)),
        x = (!!rlang::sym(pos)),
        y = -log10((!!rlang::sym(p_value)))
      ),
      segment.color = "grey50",
      size=2.0,
      max.overlaps = 10,
      label.padding = 0.1,
      box.padding = 0.5)+
    ggplot2::theme_bw() +
    ggplot2::scale_fill_grey(drop=TRUE) +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "black"),
      legend.key.size = grid::unit(0.4, 'cm')
    )+
    ggplot2::ylab(sprintf("-log(%s)", p_value))+
    ggplot2::scale_y_continuous(
      limits = c(0,2.0),
      expand = c(0,0))+
    ggplot2::coord_fixed(ratio = coord_ratio)

  return(g)
}
