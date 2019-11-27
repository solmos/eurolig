#' Plot shotcharts
#'
#' \code{plotShotchart} plots where shots were taken on the court.
#'
#' @param shots A shot location data frame.
#'
#' @return A ggolot object.
#' @export
#'
#' @examples
plotShotchart <- function(shots) {
    court <- construct_court()
    ggplot(shots, aes_(~ coord_x, ~ coord_y)) +
        geom_point(aes_(color = ~ make), alpha = 0.8) +
        geom_path(data = court$outer_lines, aes_(~ x, ~ y)) +
        geom_path(data = court$paint, aes_(~ x, ~ y)) +
        geom_path(data = court$arc3, aes_(~ x, ~ y)) +
        geom_path(data = court$ft_circle, aes_(~ x, ~y)) +
        geom_path(data = court$backboard, aes_(~ x, ~y)) +
        geom_path(data = court$rim, aes_(~ x, ~y)) +
        geom_path(data = court$restricted_area, aes_(~ x, ~y)) +
        geom_path(data = court$middle_circle, aes_(~ x, ~y)) +
        coord_fixed() +
        ggplot2::theme_void()
}

#' Construct coordinates of basketball court
#'
#' @return A list containing data frames corresponding to different parts of
#'   the court
#' @export
#'
#' @examples
#' court <- construct_court()
construct_court <- function() {
    outer_lines <- data.frame(
        x = c(-7.5, -7.5, 7.5, 7.5, -7.5),
        y = c(0, 14, 14, 0, 0),
        type = "Outer lines"
    )

    paint <- data.frame(
        x = c(-2.45, -2.45, 2.45, 2.45),
        y = c(0, 5.8, 5.8, 0),
        type = "Paint"
    )

    ft_circle <- data.frame(
        construct_arc(x0 = 0, y0 = 5.8, r = 1.8, start = 0, stop = pi),
        type = "FT circle"
    )

    # The 3pt line transforms into a straight line in the corners
    # Precisely, it transforms to a vertical line when the x coordinates
    # of the arc are above or below 6.6 and -6.6 respectively.
    upper_arc3 <- data.frame(
        construct_arc(x0 = 0, y0 = 1.575, r = 6.75, start = 0, stop = pi),
        type = "Upper arc"
    ) %>%
        dplyr::filter(abs(.data$x) <= 6.6)

    # To find the y coordinate where the vertical line in the corner and
    # the 3pt arc meet, we just find the minimum value of the arc above
    y_max_corner <- min(upper_arc3$y)
    left_corner3 <- data.frame(
        x = c(-6.6, -6.6),
        y = c(0, y_max_corner),
        type = "Left corner 3"
    )
    right_corner3 <- data.frame(
        x = c(6.6, 6.6),
        y = c(y_max_corner, 0),
        type = "Right corner 3"
    )
    arc3 <- rbind(right_corner3, upper_arc3, left_corner3)

    backboard <- data.frame(
        x = c(-0.9, 0.9),
        y = c(1.2, 1.2),
        type = "backboard"
    )

    rim <- construct_arc(x0 = 0, y0 = 1.575, r = 0.225,
                         start = 0, stop = 2 * pi)

    semi_circle <- data.frame(
        construct_arc(0, 1.575, r = 1.25, 0, pi),
        type = "semi_circle"
    )
    semi_circle_left <- data.frame(
        x = c(-1.25, -1.25),
        y = c(1.575, 1.2),
        type = "semi_circle_left"
    )
    semi_circle_right <- data.frame(
        x = c(1.25, 1.25),
        y = c(1.575, 1.2),
        type = "semi_circle_right"
    )
    restricted_area <- rbind(semi_circle_right, semi_circle, semi_circle_left)

    middle_circle <- data.frame(
        construct_arc(0, 14, 1.8, pi, 2 * pi),
        type = "middle_circle"
    )

    court <- list(
        outer_lines = outer_lines,
        paint = paint,
        ft_circle = ft_circle,
        arc3 = arc3,
        backboard = backboard,
        rim = rim,
        restricted_area = restricted_area,
        middle_circle = middle_circle
    )

    court
}


#' Construct the coordinates of an arc
#'
#' @param x0 Center x coordinate
#' @param y0 Center y coordinate
#' @param r Radius
#' @param start In radians from 0 to pi
#' @param stop In radians from 0 to pi
#'
#' @keywords internal
#'
#' @return Data frame with x, y coordinates
#' @export
#'
#' @examples
#' circle <- construct_arc(x0 = 0, y0 = 0, r = 1, start = 0, stop = 2 * pi)
#' plot(circle$x, circle$y, type = "l")
#' arc <- construct_arc(2, 1, 5, start = pi, stop = pi / 2)
#' plot(arc$x, arc$y, type = "l")
construct_arc <- function(x0, y0, r, start, stop) {
    by <- ifelse(start <= stop, 0.001, -0.001)
    theta <- seq(start, stop, by)
    x <- x0 + r * cos(theta)
    y <- y0 + r * sin(theta)
    data.frame(x, y)
}
