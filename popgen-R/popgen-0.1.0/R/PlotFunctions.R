#' Plot tuning parameters
#' @export
plotTuning <- function(cross.tbl, xlabel = "Threshold", ylabel = "ARI", isNumeric = F){
  plotdf <- cbind(as.data.frame(rownames(cross.tbl)), cross.tbl)
  colnames(plotdf)[1] <- xlabel
  plotdf <- melt(plotdf, id.vars = xlabel, variable_name = "series")
  colnames(plotdf)[3] <- ylabel
  if (isNumeric) plotdf[, "Threshold"] <- as.numeric(as.character(plotdf[, "Threshold"]))

  ggplot(plotdf, aes(Threshold, ARI, colour = series, group = series)) +
    geom_line(size = 0.5) +
    geom_point() +
    theme(legend.title = element_blank(), legend.position = "top")
  #  scale_x_continuous(breaks = seq(0, 100, by = 10))
  #  geom_text(data = plotdf[plotdf$Threshold == plotdf$Threshold[nrow(plotdf)],], aes(label = series), hjust = 0.6, vjust = 1.5)
}

# Plot the result
# Input parameters: data after PCA, two PCs (i.e. PC2 and PC3, when PC2 is x and PC3 is y)

#' Plot map
#' @export
plotMap2 <- function(data, PCx, PCy){
  ggplot(data, aes(PCx, PCy, color = Region)) +
    geom_point(size=1) +
    geom_point(size=2, aes(shape=prediction), show.legend = T) +
    scale_shape_manual(values=1:nlevels(as.factor(prediction))) +
    scale_fill_identity() +
    theme(legend.title = element_blank(), legend.position = "top")
}

#' Plot map
#' @export
plotMap <- function (data, PCx, PCy){
  ui <- fluidPage(
    fluidRow(
      column(width = 12, class = "well",
             h4("Brush and double-click to zoom"),
             plotOutput("plot1", height = 900,
                        dblclick = "plot1_dblclick",
                        brush = brushOpts(
                          id = "plot1_brush",
                          resetOnNew = TRUE
                        )
             )
      )
    )
  )

  server <- function(input, output) {

    # -------------------------------------------------------------------
    # Single zoomable plot (on left)
    ranges <- reactiveValues(x = NULL, y = NULL)

    output$plot1 <- renderPlot({
      ggplot(data, aes(PCx, PCy, color = Region)) +
        geom_point() +
        geom_point(size=8, aes(shape=prediction), show.legend = T) +
        scale_shape_manual(values=1:nlevels(as.factor(prediction))) +
        scale_fill_identity() +
        theme(legend.title = element_blank(), legend.text=element_text(size=20), legend.position = "top",
              axis.text=element_text(size=16),
              axis.title=element_text(size=18,face="bold")) +
        # geom_text(aes(label = prediction), hjust=0, vjust=0) + # Region
        coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)

      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
  }

  shinyApp(ui, server)
}
