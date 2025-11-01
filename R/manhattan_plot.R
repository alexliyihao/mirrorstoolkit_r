#' Clean format for a Manhattan plot
#'
#' A function cleans column format for a Manhattan plot, in which "phenotype" and "coding" should be factor, "pos" and "p-value" should be numerical, formal name should be character
#'
#' @param data, data.frame, The table working on
#' @param pos, string, the name of column of position of the variant
#' @param phenotype, str, the name of column of phenotype the variant associated to
#' @param p_value, str, the name of column of p-values
#' @param formal_name, str, the name of column of formal name of each variant
#' @param coding, str, the name of column of exonic vs intronic, currently not used
#' @param phenotype_list, vector of str, the list of phenotypes which is put into consideration
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

#' Create a non-white color palette for ggplot2 based on Polychrome::glasbey.colors
#'
#' Generate a color palette for ggplot2 with specified number of color, white is intentionally removed.
#'
#' @param num_color, int, the number of color need
#' @return list of character, the color list
create_multi_color_palette = function(num_color){
  rlang::check_installed("Polychrome", reason = "to use `Polychrome::glasbey.colors()`")
  palette_table = Polychrome::glasbey.colors(num_color+1)
  names(palette_table) <- NULL
  palette_table = palette_table[palette_table != "#FFFFFF"]
  return(palette_table)
}

#' Wrapper creating a distinguishable, non-white color palette for ggplot2 based on Polychrome::glasbey.colors
#'
#' Generate a color palette for ggplot2 with specified number of color, white is intentionally removed, added codes dealt with 1 or 2 color(s).
#'
#' @param num_color, int, the number of color need
#' @return list of character, the color list
#' @export
distinguishable_palette = function(num_color){
  if (num_color == 1){
    palette_color = "black"
  } else if (num_color == 2){
    palette_color = c("skyblue2", "black")
  } else if (num_color <= 31){
    palette_color = create_multi_color_palette(num_color = num_color)
  } else {
    stop(
      sprintf(
        'The color palette cannot support more than 31 colors, has %d now',
        num_color))
  }
  return(palette_color)
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
#' @param formal_name, str, the name of column of formal name of each variant, if not specified, will use pos
#' @param coding, str, the name of column of exonic vs intronic, currently not used
#' @param phenotype_list, vector of str, the list of phenotypes which is put into consideration
#' @param highlight_region, list of vector of str in list(c(start, end)...) format, the list of highlighted region
#' @param palette_color, list of colors used for different phenotypes
#' @param coord_ratio, float, the coordinate ratio for the figure
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

  if (is.null(palette_color)){
    num_phenotype = for_figure %>%
      dplyr::pull("phenotype") %>%
      unique() %>%
      length()
    palette_color = distinguishable_palette(num_color = num_phenotype)
  }

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
    mapping = aes(
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
      mapping = aes(
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
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text = element_text(color = "black"),
      legend.key.size = unit(0.4, 'cm')
    )+
    ggplot2::ylab(sprintf("-log(%s)", p_value))+
    ggplot2::scale_y_continuous(
      limits = c(0,2.0),
      expand = c(0,0))+
    ggplot2::coord_fixed(ratio = coord_ratio)

  return(g)
}
