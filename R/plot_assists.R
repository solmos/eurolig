#' Plot assists network
#'
#' Uses igpraph to plot a network graph showing who assisted who from play-by-play data.
#'
#' @param pbp_df Data frame with play-by-play data
#' @param team
#'
#' @return
#' @export
#'
#' @examples
plot_assists <- function(pbp_df, team) {
    team_df <- dplyr::filter(pbp_df, TEAM == team)
    assist_df <- get_assists(team_df)
    edges <- assist_df %>%
        dplyr::group_by(PASSER, SHOOTER) %>%
        dplyr::count()

    nodes <- assist_df %>%
        dplyr::group_by(PASSER) %>%
        dplyr::summarise(pts_generated = sum(POINTS))

    # We need to add the players with no assists to nodes
    all_node_ids <- unique(c(as.character(edges$PASSER),
                             as.character(edges$SHOOTER)))
    non_passers <- all_node_ids[!all_node_ids %in% edges$PASSER]
    non_passers_df <- data.frame(PASSER = non_passers,
                                 pts_generated = 0)
    nodes <- rbind(nodes, non_passers_df)

    # Shorten names to just the last name of the player
    node_labels <- stringr::str_extract(all_node_ids, "[A-z]+")
    node_labels_df <- data.frame(id = all_node_ids,
                                 label = node_labels,
                                 stringsAsFactors = FALSE)

    nodes <- nodes %>%
        dplyr::transmute(id = as.character(PASSER),
                         value = pts_generated,
                         tittle = PASSER) %>%
        dplyr::left_join(node_labels_df, by = "id")

    # Igraph
    net <- igraph::graph_from_data_frame(d = edges,
                                         vertices = nodes,
                                         directed = TRUE)

    igraph::V(net)$size <- scale(nodes$value) + 20
    igraph::V(net)$color <- "slategray2"
    igraph::V(net)$frame.color <- "white"
    igraph::V(net)$label.color <- "black"
    igraph::V(net)$label.family <- "Mono"
    igraph::E(net)$width <- edges$n * 2

    plot(net, edge.arrow.size = .3, edge.curved = 0.4)
}
