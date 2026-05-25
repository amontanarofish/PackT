library(devtools)
install.packages("git2r")
#test package called PackT
dir.create("C:/Users/sodam/OneDrive - Deakin University/Git_Repositories/PackT")
setwd("C:/Users/sodam/OneDrive - Deakin University/Git_Repositories/PackT")
create_package("C:/Users/sodam/OneDrive - Deakin University/Git_Repositories/PackT")
use_git()

usethis::use_package("ggplot2")

#basic functino to multiply number by 50

x <- 50

m50 <- function(x) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  x * 50

}

m50(x)

#correlated random walk progression

distance_between_points <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

simple_random_walk <- function(n_steps = 100,
                               step_length = 1) {

  x <- numeric(n_steps)
  y <- numeric(n_steps)

  for (i in 2:n_steps) {

    angle <- runif(1, 0, 2*pi)

    x[i] <- x[i - 1] + step_length * cos(angle)
    y[i] <- y[i - 1] + step_length * sin(angle)
  }

  data.frame(x = x, y = y)
}

crw <- function(n_steps = 100,
                step_length = 1,
                turn_sd = 0.3) {

  x <- numeric(n_steps)
  y <- numeric(n_steps)
  angle <- numeric(n_steps)

  angle[1] <- runif(1, 0, 2*pi)

  for (i in 2:n_steps) {

    angle[i] <- angle[i - 1] + rnorm(1, 0, turn_sd)

    x[i] <- x[i - 1] + step_length * cos(angle[i])

    y[i] <- y[i - 1] + step_length * sin(angle[i])
  }

  data.frame(
    step = 1:n_steps,
    x = x,
    y = y
  )
}

track <- crw()
plot(track$x, track$y, type = "l", asp = 1)

###

crw <- function(n_steps = 100,
                step_length = 1,
                turn_sd = 0.3,
                target_x = 30,
                target_y = 50,
                plot_track = TRUE) {

  x <- numeric(n_steps)
  y <- numeric(n_steps)
  angle <- numeric(n_steps)
  distance_to_point <- numeric(n_steps)

  angle[1] <- runif(1, 0, 2*pi)

  # Distance for first point
  distance_to_point[1] <- sqrt(
    (x[1] - target_x)^2 +
      (y[1] - target_y)^2
  )

  for (i in 2:n_steps) {

    # Generate new direction
    angle[i] <- angle[i - 1] + rnorm(1, 0, turn_sd)

    # Move animal
    x[i] <- x[i - 1] + step_length * cos(angle[i])
    y[i] <- y[i - 1] + step_length * sin(angle[i])

    # Calculate distance to fixed point
    distance_to_point[i] <- sqrt(
      (x[i] - target_x)^2 +
        (y[i] - target_y)^2
    )
  }

  track <- data.frame(
    step = 1:n_steps,
    x = x,
    y = y,
    distance_to_point = distance_to_point
  )

  if (plot_track == TRUE) {
    closest_index <- which.min(track$distance_to_point)

    plot(track$x,
         track$y,
         type = "l",
         asp = 1,
         xlab = "X",
         ylab = "Y")

    points(target_x,
           target_y,
           col = "red",
           pch = 19,
           cex = 1.5)

    points(track$x[closest_index],
           track$y[closest_index],
           col = "blue",
           pch = 19)

    segments(
      x0 = track$x[closest_index],
      y0 = track$y[closest_index],
      x1 = target_x,
      y1 = target_y,
      col = "darkgreen",
      lwd = 2,
      lty = 2
    )
  }

  return(track)

  }

track <- crw()

crw(plot_track = TRUE)

plot(track$x,
     track$y,
     type = "l",
     asp = 1)
points(30, 50, col="red", pch = 19)

# split function

#' Simulate a correlated random walk
#'
#' @param n_steps Number of movement steps
#' @param step_length Distance moved per step
#' @param turn_sd Standard deviation of turning angle
#' @param target_x X coordinate of target point
#' @param target_y Y coordinate of target point
#'
#' @return A dataframe containing movement coordinates
#' @export
simulate_crw <- function(n_steps = 100,
                         step_length = 1,
                         turn_sd = 0.3,
                         target_x = 30,
                         target_y = 50) {

  x <- numeric(n_steps)
  y <- numeric(n_steps)
  angle <- numeric(n_steps)
  distance_to_point <- numeric(n_steps)

  angle[1] <- runif(1, 0, 2*pi)

  distance_to_point[1] <- sqrt(
    (x[1] - target_x)^2 +
      (y[1] - target_y)^2
  )

  for (i in 2:n_steps) {

    angle[i] <- angle[i - 1] + rnorm(1, 0, turn_sd)

    x[i] <- x[i - 1] + step_length * cos(angle[i])
    y[i] <- y[i - 1] + step_length * sin(angle[i])

    distance_to_point[i] <- sqrt(
      (x[i] - target_x)^2 +
        (y[i] - target_y)^2
    )
  }

  track <- data.frame(
    step = 1:n_steps,
    x = x,
    y = y,
    distance_to_point = distance_to_point
  )

  return(track)
}

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

sim <- simulate_crw()
plot_crw(sim)
