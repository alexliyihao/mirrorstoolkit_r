#' Function cleaning correlation matrix to pairwise dataframe
#'
#' This function takes a correlation matrix and a p-value matrix from correlation matrix computing
#' clean the matrix to pairwise terms
#' credit to https://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#'
#' @param corr_matrix matrix, a numerical matrix, the correlation matrix
#' @param p_matrix matrix, a numerical matrix, the p-value matrix
#'
#' @return data.frame, a data.frame with two columns: corr and p_value
flatten_correlation_matrix = function(corr_matrix, p_matrix) {
  ut <- base::upper.tri(corr_matrix)
  output = data.frame(
    row = rownames(corr_matrix)[row(corr_matrix)[ut]],
    column = rownames(corr_matrix)[col(corr_matrix)[ut]],
    corr = (corr_matrix)[ut],
    p_value = p_matrix[ut]
  )
  return(output)
}

#' Function cleaning input and computing correlation matrix
#'
#' If no variable list provided, the function will clean all the value as numerical matrix
#' and compute the correlation matrix
#'
#' @param data data.frame, the data table.
#' @param variable optional character vector, the list of variable included in computation, all columns by default
#'
#' @return a list with elements r, the matrix of correlations, n the matrix of number of observations used in analyzing each pair of variables, P, the asymptotic P-values, and type.
clean_correlation_matrix = function(data, variable=NULL){
  if(!is.null(variable)){
    data = data %>% dplyr::select(
      tidyr::all_of(variable)
    )
  }
  correlation_matrix = data %>%
    dplyr::mutate(
      dplyr::across(
        tidyr::everything(),
        base::as.numeric)) %>%
    base::as.matrix() %>%
    Hmisc::rcorr()
  return(correlation_matrix)
}

#' Function wrapper compute pairwise correlation matrix and return as a data.frame
#'
#' If no variable list provided, the function will clean all the value as numerical matrix
#' and compute the correlation matrix
#'
#' @param data data.frame, the data table.
#' @param variable optional character vector, the list of variable included in computation, all columns by default
#' @param p_threshold optional float, the only the rows with p-value less than this value will be included, default 1 (all included)
#'
#' @return data.frame, a data.frame with two columns: corr and p_value
#' @export
correlation_matrix_dataframe = function(data, variable = NULL, p_threshold = 1) {
  correlation_matrix = clean_correlation_matrix(
    data = data,
    variable = variable)
  pairwise_correlation = flatten_correlation_matrix(
    corr_matrix = correlation_matrix$r,
    p_matrix = correlation_matrix$P) %>%
    dplyr::filter(p_value <= p_threshold)
  return(pairwise_correlation)
}

#' Function wrapper compute pairwise correlation matrix and return as a visualization format
#'
#' If no variable list provided, the function will clean all the value as numerical matrix
#' and compute the correlation matrix
#'
#' @param data data.frame, the data table.
#' @param variable optional character vector, the list of variable included in computation, all columns by default
#' @param p_threshold optional float, the only the rows with p-value less than this value will be included, Null default(all included)
#' @param font_size optional int, the size of row/column labels, default 5
#'
#' @return data.frame, a data.frame with two columns: corr and p_value
#' @export
correlation_matrix_plot = function(data, variable = NULL, p_threshold = NULL, font_size = 5) {
  correlation_matrix = clean_correlation_matrix(
    data = data,
    variable = variable)
  if (is.null(p_threshold)){
    correlation_matrix_figure = ggcorrplot::ggcorrplot(
      corr = correlation_matrix$r,
      tl.cex = font_size,
      hc.order = TRUE)
  } else {
    correlation_matrix_figure = ggcorrplot::ggcorrplot(
      corr = correlation_matrix$r,
      tl.cex = font_size,
      p.mat = correlation_matrix$P,
      sig.level = p_threshold,
      insig = "blank",
      type="upper",
      hc.order = TRUE)
  }
  return(correlation_matrix_figure)
}


