#' 3d visualization for rms fit object
#'
#' A wrapper generating plotly figures for rms result in 3d surface, mimicking the behaviour for ggrmsMD in 2D
#'
#' @param fit, rms model fit result
#' @param datadist, rms datadist() result
#' @param variable, vector of str with length 2, the two variables on x and y axis
#' @param target_name, str, the axis label for prediction on z axis, default "target"
#' @param density, int or vector of int with length 2, the number of points on x and y axis, if only given one number will deployed on both axes, default 50
#' @return plotly::plot_ly instance, the 3d output for the model
#' @export
rms_illustration_3d = function(
    fit,
    datadist,
    variable,
    target_name = "target",
    density = 50){
  overall_distribution = datadist$limits %>% data.table::copy()
  if (length(variable) != 2){
    print("The length of variable must be 2")
    return(NULL)
  }
  if (length(density) == 1){
    density = c(density, density)
  }
  # create a grid of x,y values
  x_axis = base::seq(
    overall_distribution["Low",variable[1]],
    overall_distribution["High",variable[1]],
    length.out = density[1])
  y_axis = base::seq(
    overall_distribution["Low",variable[2]],
    overall_distribution["High",variable[2]],
    length.out = density[2])
  input = base::expand.grid(
    x_axis = x_axis,
    y_axis = y_axis) %>%
    magrittr::set_colnames(variable)

  # add adjustment variables
  adjustments = base::setdiff(
    overall_distribution %>% base::colnames(),
    variable)
  for (adjustment in adjustments){
    input = input %>% dplyr::mutate(
      !!adjustment := overall_distribution["Adjust to", adjustment])
  }
  # Compute prediction for input grid
  prediction = cbind(
    input %>%
      dplyr::select(tidyr::all_of(variable)),
    stats::predict(fit, input)
  ) %>% magrittr::set_colnames(c(variable, "prediction"))
  output = prediction %>%
    tidyr::pivot_wider(
      id_cols = !!rlang::sym(variable[1]),
      names_from = !!rlang::sym(variable[2]),
      values_from = !!rlang::sym("prediction")) %>%
    tibble::column_to_rownames(variable[1])
  # plotly used a very weird x y z coordinate specification,
  # left it as-is, z should be transposed.
  p = plotly::plot_ly(
    x = ~x_axis,
    y = ~y_axis,
    z = output %>% as.matrix() %>% t(),
    type = "surface"
  ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = variable[1]),
        yaxis = list(title = variable[2]),
        zaxis = list(title = target_name)
      ))
  return(p)
}
