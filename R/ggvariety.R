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
#' ggvariety(variety, c(-2,2), c(-2,2), 20)
#' 
#' # Alpha curve (not smooth)
#' ggvariety(mp("y^2-x^3-x^2"), c(-2,2), c(-2,2), 50)
#' 
#' # Alpha curve (smooth)
#' ggvariety(mp("y^2-x^3-x^2"), c(-2,2), c(-2,2), 400)
#' 
#' 

ggvariety <- function(mp, xlim, ylim, obs = 101, ...) {
  # make a data frame
  df <- data_frame(
    x = seq(xlim[1], xlim[2], length.out = obs), 
    y = seq(ylim[1], ylim[2], length.out = obs)
  )
  df <- cross_df(df)
  df <- mutate(df, z = as.function(mp, varorder = c("x", "y"), 
               vector = FALSE, silent = TRUE)(x,y))

  # make the plot using geom_contour()
  ggplot(df, aes(x,y,z=z)) + geom_contour(breaks = 0) + coord_equal()
}


# look at mesh, adaptively refine to add more points to 
# df near places where you think a crossing occurs




