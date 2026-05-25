#' Plot a correlated random walk
#'
#' @param track Dataframe produced by simulate_crw()
#' @param target_x X coordinate of target point
#' @param target_y Y coordinate of target point
#'
#' @return A ggplot object
#' @export
plot_crw <- function(track,
                     target_x = 30,
                     target_y = 50) {

  library(ggplot2)

  # Find closest point
  closest_index <- which.min(track$distance_to_point)

  closest_point <- track[closest_index, ]

  ggplot(track, aes(x = x, y = y)) +

    # Track line
    geom_path(linewidth = 1) +

    # Target point
    geom_point(
      aes(x = target_x, y = target_y),
      colour = "red",
      size = 4
    ) +

    # Closest point
    geom_point(
      data = closest_point,
      colour = "blue",
      size = 3
    ) +

    # Connecting line
    geom_segment(
      aes(
        x = closest_point$x,
        y = closest_point$y,
        xend = target_x,
        yend = target_y
      ),
      colour = "darkgreen",
      linewidth = 1,
      linetype = "dashed"
    ) +

    # Fixed axes
    coord_fixed(
      xlim = c(-100, 100),
      ylim = c(-100, 100)
    ) +

    labs(
      title = "Correlated Random Walk",
      x = "X coordinate",
      y = "Y coordinate"
    ) +

    theme_minimal()
}
