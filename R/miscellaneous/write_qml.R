#' @name write_qml
#' 
#' @title Write a raster qml file. 
#' 
#' @description Given a vector of colors and breaks a styles 
#' 
#' @param colors a vector of colors
#' @param values a vector of values
#' @param labels a vector of labels for the legend
#' @param filename a file name, usually with the extension \".qml\"
#' @seealso \code{\link{divColsAndBreaks}}, \code{\link{rgb24bit}} 
#' @references As a template for this function a nice post available online helped a lot. 
#' This post describes the creation of a qml legend file for shape files:   
#' \url{http://pvanb.wordpress.com/2012/07/02/from-attribute-table-to-qgis-style-file-step-2/}
#' @examples
#' \dontrun{
#' ### to do
#' }
#' @export
write_qml <- function (colors, values, labels=NULL, filename) {
  
  require(brew)
  
  if (length(colors)!=length(values))
    stop("Length of color vector must be qual than the vector of values.")
  # Name and path of style file
  xml.out <- file(filename,"w")
  
  # Write header information to the style file
  #-----------------------------------------------------------
  
  brew(text=paste(
    "<qgis version=\"2.4.0-Chugiak\" minimumScale=\"0\" maximumScale=\"1e+08\" hasScaleBasedVisibilityFlag=\"0\">
    <pipe>
    <rasterrenderer opacity=\"1\" alphaBand=\"-1\" classificationMax=\"", max(values), "\" classificationMinMaxOrigin=\"MinMaxFullExtentEstimated\" band=\"1\" classificationMin=\"", min(values), "\" type=\"singlebandpseudocolor\">
    <rasterTransparency/>
    <rastershader>
    <colorrampshader colorRampType=\"INTERPOLATED\" clip=\"0\">", 
    sep=""), output=xml.out)
  
  # Write the categories definitions to the style file
  #-----------------------------------------------------------
  
  if (is.null(labels[1]))
    labels <- values
  
  #   brew(text=paste(
  #     "<item alpha=\"255\"  value=\"<%=Value%>\" label=\"<%=Label%>\" color=\"<%=Color%>\"/>
  #          <%-%>", sep=""), output=xml.out)
  #   
  for(i in 1:length(colors)){
    Color <- colors[i]
    Value <- values[i]
    Label <- labels[i]
    brew(text=paste(
      "<item alpha=\"255\"  value=\"<%=Value%>\" label=\"<%=Label%>\" color=\"<%=Color%>\"/>
      <%-%>", sep=""), output=xml.out)
  }
  brew(text=paste(
    "<item alpha=\"255\"  value=\"<%=Value%>\" label=\"<%=Label%>\" color=\"<%=Color%>\"/>
    <%-%>", sep=""), output=xml.out)
  
  
  # Write closing lines to the footer
  #-----------------------------------------------------------
  brew(text=paste(
    "</colorrampshader>
    </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness=\"0\" contrast=\"0\"/>
    <huesaturation colorizeGreen=\"128\" colorizeOn=\"0\" colorizeRed=\"255\" colorizeBlue=\"128\" grayscaleMode=\"0\" saturation=\"0\" colorizeStrength=\"100\"/>
    <rasterresampler maxOversampling=\"2\"/>
    </pipe>
    <blendMode>0</blendMode>
    </qgis>"
    , sep=""), output=xml.out)
  close(xml.out)
  }