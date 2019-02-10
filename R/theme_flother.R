#' A ggplot2 theme in the style used on https://flother.is/
#'
#' @param font_family Font family for all text, by default Matthew Butterick's
#'                    Valkyrie (see \url{https://practicaltypography.com/valkyrie.html}).
#' @param font_size Base size of font. The size of all text, lines, and margins
#'                  is relative to this.
#' @return A complete ggplot2 theme.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(USArrests, aes(Murder, UrbanPop)) +
#'   geom_point() +
#'   theme_flother()
theme_flother <- function(font_family = "Valkyrie T4", font_size = 12) {
  ggplot2::theme(
    # Set foundation for all text elements.
    text = ggplot2::element_text(family = font_family,
                                 face = "plain",
                                 colour = "#222222",
                                 hjust = 0,
                                 vjust = 0,
                                 angle = 0,
                                 lineheight = 1.2,
                                 margin = ggplot2::margin(),
                                 size = font_size,
                                 debug = FALSE),

    # The plot is the underlying canvas.
    plot.background = ggplot2::element_rect(fill = "white",
                                            colour = NA),
    plot.margin = ggplot2::margin(font_size, font_size, font_size, font_size),
    plot.title = ggplot2::element_text(face = "bold",
                                       size = ggplot2::rel(1.4),
                                       margin = ggplot2::margin(b = font_size * 0.5)),
    plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = font_size * 2)),
    plot.caption = ggplot2::element_text(colour = "#999999",
                                         size = ggplot2::rel(0.9),
                                         margin = ggplot2::margin(font_size)),

    # The panel is the plotting area. It contains the data and gridlines but not
    # the legend or axes (nor the axis ticks, labels, and titles).
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.ontop = FALSE,
    panel.grid = ggplot2::element_line(colour = "#dddddd",
                                       size = 0.5,
                                       linetype = "solid",
                                       lineend = "round"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_line(size = ggplot2::rel(0.5)),
    panel.spacing = grid::unit(font_size, "points"),

    # Axis lines that outline the panel.
    axis.line.x = ggplot2::element_line(colour = "#222222"),
    axis.line.y = ggplot2::element_blank(),

    # X- and Y-axis titles.
    axis.title = ggplot2::element_text(hjust = 0.5),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = font_size)),
    axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = font_size)),
    axis.title.y = ggplot2::element_text(angle = 0,
                                         vjust = 1,
                                         hjust = 1),
    axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = font_size)),
    axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(r = font_size)),

    # Data value labels along the x- and y-axis.
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
    axis.text.x = ggplot2::element_text(hjust = 0.5),
    axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = font_size * 0.5)),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = font_size * 0.5)),
    axis.text.y = ggplot2::element_text(hjust = 0),

    # Tick marks that link labels to their locations on the axis lines.
    axis.ticks = ggplot2::element_line(colour = "#222222",
                                       size = 0.5,
                                       lineend = "round"),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.length = grid::unit(font_size * 0.25, 'points'),

    # Settings for the part of the legend concerned with individual data series.
    legend.background = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(l = -font_size),
    legend.spacing.x = grid::unit(font_size, "points"),
    legend.spacing.y = grid::unit(font_size, "points"),
    legend.key = ggplot2::element_blank(),
    legend.key.height = grid::unit(1, "points"),
    legend.key.width = grid::unit(font_size * 2, "points"),
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
    legend.text.align = 0,
    legend.title = ggplot2::element_text(face = "bold"),
    legend.title.align = 0,

    # Settings for the legend as a whole (which may contain legends for multiple
    # data series).
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.box.margin = ggplot2::margin(0),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::margin(0),

    # Title bar for individual facets.
    strip.background = ggplot2::element_blank(),
    strip.placement = "outside",
    strip.text = ggplot2::element_text(face = "bold",
                                       margin = ggplot2::margin(t = font_size,
                                                                b = font_size)),
    strip.switch.pad.grid = grid::unit(0, "points"),
    strip.switch.pad.wrap = grid::unit(0, "points"),

    # Ensures that everything inherits from `element_blank()`, so we're working
    # with a blank canvas.
    complete = TRUE
  )
}
