#' Function to create a plot for the spatstat class ‘fv’, combined with 'Quantum Plots' from:
#' Esser, D. S., Leveau, J. H. J., Meyer, K. M., & Wiegand, K. (2014). Spatial scales of interactions among bacteria and between bacteria and the leaf surface. FEMS Microbiology Ecology, 91(3), fiu034. http://doi.org/10.1093/femsec/fiu034
#' The colored bands highlight the spatial scales at which the summary statistics deviate from the simulation envelopes.
#' 
#' @param x a spatstat fv object
#' @param colour A vector of standard colors

#' @import ggplot2, ggthemes
#' @import spatstat
#' @return returns A plot
#' @export


quantumPlot <- function(x,colour=c("#d73027", "#ffffbf", "#91bfdb")){
  
  # load Packages
  require(ggplot2)
  require(ggthemes)
  
  # convert fv to dataframe
  env.data <- as.data.frame(tree.data)
  env.data <- env.data[-1,]
  
  # plot it
  gg_quantomPlot <- ggplot(env.data, aes(r, obs))+
    # plot observed value 
    geom_line(colour=c("#4d4d4d"))+
    # plot simulation envelopes
    geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.1, colour=c("#e0e0e0")) +
    # axes names and limits
    ylim(min(env.data$obs)-1, max(env.data$obs)+2) + 
    xlab("Distance r (m)") +
    ylab("summary statistic") +
    # plot expected value, according to null model
    geom_hline(yintercept=1, linetype = "dashed", colour=c("#999999")) +
    # plot 'Quantums'
    geom_rug(data=env.data[env.data$obs > env.data$hi,], sides="b", colour=colour[1])  +
    geom_rug(data=env.data[env.data$obs < env.data$lo,], sides="b", colour=colour[2]) + 
    geom_rug(data=env.data[env.data$obs >= env.data$lo & env.data$obs <= env.data$hi,], sides="b", color=colour[3]) +
    # make it look beautiful 
    theme_tufte() 
  return(gg_quantomPlot)
}
