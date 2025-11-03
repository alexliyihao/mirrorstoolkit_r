#' NIH 2 equation, also known as the Sampson-NIH equation
#'
#' Compute the LDL(calc) based on NIH equation 2
#'
#' @param tchol, float, total cholesterol
#' @param hdl, float HDL level
#' @param trig, float Triglyceride level
#'
#' @return float, the LDL level
#' @export
compute_nih_ldl = function(tchol, hdl, trig){
  nonhdl = tchol-hdl
  ldl = tchol/0.948 - hdl/0.971 - (trig/8.56 + trig*nonhdl/2140 - trig^2/16100)-9.44
  return(ldl)
}
