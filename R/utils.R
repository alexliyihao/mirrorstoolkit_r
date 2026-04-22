#' Create a non-white color palette for ggplot2 based on Polychrome::glasbey.colors
#'
#' Generate a color palette for ggplot2 with specified number of color, white is intentionally removed.
#'
#' @param num_color, int, the number of color need
#'
#' @return list of str, the color list
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
#' @param pre_specified_palette, vector of str, the palette provided, if provided, will directly output this vector
#'
#' @return list of str, the color list
#' @export
distinguishable_palette = function(num_color, pre_specified_palette = NULL){
  # this is only for DRY practice, if user specified it, let it override
  # don't want to check it in main function everytime...
  if (!is.null(pre_specified_palette)){
    return(pre_specified_palette)
  }
  # If it's 1, stick with black, 2, skyblue and black,
  # 3-31 use Polychrome::glasbey.colors (dropped a leading white for the 32, which is absurd in ggplot)
  # 31+ throw an error
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

#' Compute OR from effect size in logistic regression
#'
#' Generate an "Odd Ratio" columns from effect size, and put it after effect size
#'
#' @param data data.frame, the table work on
#' @param effect_size_name str, the name of column of effect size
#' @param odd_ratio_name str, the name of new column generated as odd ratio
#'
#' @return data.frame with an additional odds_ratio
#' @export
generate_odd_ratio = function(
    data,
    effect_size_name = "Effect Size",
    odd_ratio_name = "Odd Ratio"){
  data = data %>%
    dplyr::mutate(!!rlang::sym(odd_ratio_name) := .data[[effect_size_name]] %>% base::exp()) %>%
    dplyr::relocate(tidyr::all_of(odd_ratio_name), .after = tidyr::all_of(effect_size_name))
  return(data)
}

