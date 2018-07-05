#' Plot a variety 
#' 
#' Uses geom_contour() and ggplot() to plot an mpoly 
#' object representing a variety in 2D space.
#' 
#' @param mp an mpoly object
#' @param xlim vector representing x bounds
#' @param ylim vector representing y bounds
#' @param obs number of ?????? 
#' @param ... additional parameters
#' @return A ggplot object containing variety plot
#' @usage \method{ggvariety}{mpoly}(mp, xlim, ylim, obs, ...)
#' @export
#' @examples
#' 
#' # Simple linear variety
#' variety <- mp("y-x+2")
#' ggvariety(variety, c(-2,2), c(-2,2))
#' 
#' # Alpha curve
#' ggvariety("y^2-x^3-x^2", xlim = c(-2, 2), ylim = c(-2, 2), 501)
#' 
#' # Alpha curve (smooth)
#' ggvariety("y^2-x^3-x^2", xlim = c(-2, 2), ylim = c(-2, 2)) + 
#' coord_equal()
#' 
#' 

ggvariety <- function(mp, xlim, ylim, nx = 101, ny = nx, ...) {
  # Check if mp argument was mpoly obj
  require("mpoly")
  if (!is.mpoly(mp)) mp <- mpoly::mp(mp)
  f <- as.function(mp, varorder = c("x", "y"), vector = FALSE, silent = TRUE)
  
  # make a data frame
  require("dplyr"); require("purrr")
  df <- data_frame(
    x = seq(xlim[1], xlim[2], length.out = nx), 
    y = seq(ylim[1], ylim[2], length.out = ny)
  ) %>% 
    cross_df() %>% 
    mutate(z = f(x,y))

  # make the plot using geom_contour()
  require("ggplot2")
  ggplot(df, aes(x, y, z = z)) + 
    geom_contour(breaks = 0)
}


# look at mesh, adaptively refine to add more points to 
# df near places where you think a crossing occurs




