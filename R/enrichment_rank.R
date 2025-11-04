#' Distribution illustration for proteomics/lipidomics, etc
#'
#' A wrapper cleaning the output from Python pipeline (TODO)
#'
#' @param data data.frame carries the enrichment
#' @param formal_name_table data.frame, the table including the formal name and save name of the output
#' @param id optional str, the column name in data referring to the subject id, by default the first column of data
#' @param save_name_col optional str, the column name in formal_name, taking the save_name from pipeline output
#' @param formal_name_col optional str, the column name in formal_name, taking the formal name in illustration
#' @param sort_policy, optional str, the sort policy of enrichments, by default "median", "mean" provided
#' @param scale, optional str, the scale of enrichments, by default "raw", "logged" provided as log10(x+1)
#' @return a list with two element, "visualization" is a ggplot2 instance as a scatter plot, "data" is the source data
#' @export
enrichment_rank = function(
    data,
    formal_name_table,
    id = NULL,
    save_name_col = NULL,
    formal_name_col = NULL,
    sort_policy = "median",
    scale = "raw"){
  # Fill the default id, save_name_col, formal_name_col if not provided
  if (is.null(id)){
    id = colnames(data)[1] # id is the first column of data
  }
  if (is.null(save_name_col)){
    save_name_col =  colnames(formal_name)[1] # save_name_col is the first column of formal_name
  }
  if (is.null(formal_name_col)){
    formal_name_col = colnames(formal_name)[2] # formal_name_col is the second column of formal_name
  }
  if (sort_policy == "median"){
    sort_policy = stats::median
  } else if (sort_policy == "mean"){
    sort_policy = base::mean
  } else {
    stop("invalid sort policy")
  }

  formal_name_table = formal_name_table %>%
    dplyr::filter(!is.na(!!rlang::sym(save_name_col))) %>%
    dplyr::mutate(
      !!rlang::sym(formal_name_col) := dplyr::coalesce(
        !!rlang::sym(formal_name_col),
        !!rlang::sym(save_name_col))
    )

  df = abundances %>%
    tidyr::pivot_longer(
      -c(id),
      names_to = "enrichments",
      values_to = "value") %>%
    dplyr::mutate(
      value = value %>% tidyr::replace_na(0),
      enrichments = enrichments %>%
        plyr::mapvalues(
          from = formal_name_table %>% dplyr::pull(save_name_col),
          to = formal_name_table %>% dplyr::pull(formal_name_col)
        ) %>%
        forcats::fct_reorder(
          value,
          .fun = sort_policy)
    )
  if (scale == "raw"){
    g = ggplot2::ggplot(data = df)+
      ggplot2::geom_point(
        mapping = aes(
          x = value,
          y = enrichments))
  } else if(scale == "logged"){
    g = ggplot2::ggplot(
      data = df %>%
        dplyr::mutate(logged_enrichment = base::log10(value + 1))
    )+
      ggplot2::geom_point(
        mapping = aes(
          x = logged_enrichment,
          y = enrichments))+
      ggplot2::xlab("log(value)")
  }

  return(list("visualization" = g, "data" = df))
}
