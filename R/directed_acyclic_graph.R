utils::globalVariables(c("id", "from_type","to_type", "p", "sd", "group"))
#' Generate and preview a directed acyclic graph for multi-omics result
#'
#' A wrapper prepare the data for a directed acyclic graph for multi-omics data, and preview it
#'
#' @param data data.frame, the data input
#' @param from str, the column name for the starting point
#' @param to str, the column name for the end point
#' @param beta str, the column name for the beta
#' @param p_value, str, the column name for p-value in the association study
#' @param from_class, str, the column name for the class label of from column if specified
#' @param to_class, str, the column name for the class label of to column if specified
#' @param class_level, optional vector of str, if need to specify a order for classes defined in from_class and to_class
#' @param color_palette, optional vector of str, the color for each class, the package will prepare a default color if not specified.
#'
#' @return A list of result with following attributes:
#'
#'  * nodes data.frame, the nodes table
#'  * edges data.frame, the edges table
#'  * illustration_network visNetwork instance, the network in illustration
#'  * raw_network visNetwork instance, the network without any visNetwork::visPhysics settings

#' @export
multi_omics_directed_acyclic_graph = function(
    data,
    from,
    to,
    beta,
    p_value,
    from_class,
    to_class,
    class_level = NULL,
    color_palette = NULL){
  # Gather possible id-group info via both "from" and "to" side
  nodes = rbind(
    data %>%
      dplyr::select(tidyr::all_of(c(from, from_class))) %>%
      magrittr::set_colnames(c("id", "group")),
    data %>%
      dplyr::select(tidyr::all_of(c(to, to_class))) %>%
      magrittr::set_colnames(c("id", "group"))
  ) %>% base::unique()
  # if no class_level given, generate a default one (by the appearance)
  if(is.null(class_level)){
    class_level = nodes %>% dplyr::pull("group") %>% base::unique()
  }
  # clean the format of the data and prepare the node info for visNetwork
  nodes = nodes %>%
    dplyr::mutate(
      group = group %>% base::factor(levels = class_level),
      level = group %>% base::as.integer(),
      label = id,
      title = base::paste0("<p><b>",group,"</b><br>",id,"</p>")
    )
  # Generate info from each row, classify the color by positive and naegatively association,
  # clean the format.
  edges = data %>%
    dplyr::select(tidyr::all_of(
      c(from, from_class, to, to_class, beta, p_value))) %>%
    magrittr::set_colnames(c("from", "from_type", "to", "to_type", "beta", "p")) %>%
    dplyr::mutate(
      arrows = "to",
      color = (beta>0) %>%
        plyr::mapvalues(
          from = c(TRUE, FALSE),
          to = c("lightpink", "lightblue")),
      title = paste0(
        "<p><b>from ",from, "(", from_type, ")",
        " to ", to, "(", to_type,")",
        "</b><br> beta = ",beta %>% formatC(format = "e"),
        " FDR-adjusted p-value = ", p %>% formatC(format = "e"),
        "</p>")
    ) %>%
    dplyr::select(-c(beta,p, from_type, to_type))
  #define color palette if not specified
  color_palette = distinguishable_palette(
    num_color = class_level %>% dplyr::n_distinct(),
    pre_specified_palette = color_palette)
  #build the palette
  network = visNetwork::visNetwork(nodes, edges, width = "100%")
  for (i in 1:(class_level %>% dplyr::n_distinct())){
    network = network %>% visNetwork::visGroups(
      groupname = class_level[i],
      color = color_palette[i]
    )
  }
  illustration_network = network %>%
    visNetwork::visLegend(
      width = 0.1,
      position = "right",
      main = "Group") %>%
    visNetwork::visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(gravitationalConstant = -100))

  return(
    list(
      "nodes"=nodes,
      "edges"=edges,
      "illustration_network"=illustration_network,
      "raw_network" = network))
}
