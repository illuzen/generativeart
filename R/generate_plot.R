#' Generate one generative plot
#'
#' This function plots previously created data.
#' @param df the data frame created with `generate_data()`
#' @param file_name the file name for saving the plot
#' @param polar do you want to use a polar coordinate system? The default is a cartesian coordinate system.
#' @param file_name filetype of the final image. Default is `png`, for other options see the `devics` argument in `ggplot::gsave()`
#' @param color color of the points. default is black
#' @param background_color background color of the plaut. default is white.
#' @param filetype set the file type for the image
#' @return a png file
#' @seealso \code{\link{generate_data}} where the data is created
#' @export
#' @examples
#' generate_plot(df, file_name, polar = FALSE)
#' @import ggplot2
#' @importFrom magrittr %>%

generate_plot <- function(df, file_name, polar, filetype, color = "black", background_color = "white", title = NULL) {
  print("generate plot")
  # faint_color <- paste(color, '00')
  faint_color <- color
  title_element <- element_text(colour = faint_color, face = 'bold', size = 256, hjust = 0.5, vjust = 0.5)
  #plot_background <- element_text(colour = faint_color, face = 'bold', size = 256, hjust = 0.5, vjust = 0.5)
  plot_background <- element_rect(fill = background_color, size = 0)
  if (polar == TRUE) {
    plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color) +
      ggplot2::theme_void() +
      ggplot2::ggtitle(title) + 
      ggplot2::theme(
	plot.background = plot_background,
	plot.title = title_element,
	plot.title.position = 'plot'
        ) +
      ggplot2::coord_fixed() +
      ggplot2::coord_polar()
  } else {
    plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color) +
      ggplot2::theme_void() +
      ggplot2::ggtitle(title) + 
      ggplot2::theme(
	plot.background = plot_background,
	plot.title = title_element,
	plot.title.position = 'plot'
        ) +
      ggplot2::coord_fixed()
  }
  ggplot2::ggsave(plot, filename = paste0(IMG_PATH, file_name), width=7, height=7, device = filetype, bg = background_color)
  print("image saved...")
}
