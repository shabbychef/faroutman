# /usr/bin/r
#
# Created: 2018.06.13
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav <steven@gilgamath.com>
# Comments: Steven E. Pav

# nonsense to get CRAN checks and NSE to play nice.
utils::globalVariables(c("fix","me"))

#' @title fractal_app .
#'
#' @description 
#'
#' A shiny app to view fractals.
#'
#' @return a shiny app.
#'
#' @keywords shiny
#' @template etc
#' @examples 
#' \dontrun{
#' fractal_app()
#' }
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot geom_tile coord_equal theme_void geom_raster guides
#' @importFrom viridis scale_fill_viridis
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @export
fractal_app <- function() {
# 2FIX: parametrize these
	ar <- 1.667
	ywid <- 2.5
	init_x <<- c(-0.5-ywid*ar/2,-0.5+ywid*ar/2)
	init_y <<- (ywid/2) * c(-1,1)

	# define UI logic
	my_ui <- shinyUI(
	fluidPage(theme=shinythemes::shinytheme("spacelab"),
	tags$head(
				tags$script(src='test.js'),
				tags$style(".table .alignRight {color: black; text-align:right;}"),
				tags$link(rel="stylesheet", type="text/css", href="style.css")
	),
	titlePanel("faroutman"),
	sidebarLayout(position="left",
								sidebarPanel(helpText('Brush plot then double click to zoom in. ',
																			'Double click without brush to reset. '),
														 hr(),
														 sliderInput("resolution","Depth: ",min=10,max=2400,value=85,step=5),
														 sliderInput("dpi", "Pixel Resolution: ",min=50,max=250,value=100,step=10),
														 hr(),
														 actionButton("panleft","pan left"),
														 actionButton("zoomout","zoom out"),
														 actionButton("panright","pan right"),
														 hr(),
														 selectInput("fractal","Fractal: ",
																				 choices=c('Mandelbroit','Fibonacci','Cos','Exp'),
																				 selected='Mandelbroit'),
														 selectInput("colorscheme","Color Scheme: ",
																				 choices=c('magma','inferno','plasma','viridis','cividis'),
																				 selected='inferno'),
														 hr(),
														 bookmarkButton(id='bookmark'),
														 hr(),
														 width=2),
    mainPanel(
			width=10,
			plotOutput('theplot',
								 dblclick="plot_click",
								 brush=brushOpts(id="plot_brush",resetOnNew=TRUE),
								 height='100%',width='100%')
			)  # mainPanel
  ) # sidebarLayout
	,title="Fractal Viewer"))

	mandeldeez <- function(x,y,maxit=100,fractal=c('Mandelbroit','Fibonacci','Cos','Exp')) {
		fractal <- match.arg(fractal)
		retv <- switch(tolower(fractal),
									 mandelbroit=mandelbrot_esc(x,y,maxit=maxit,escape=4.0),
									 fibonacci=fibonacci_esc(x,y,maxit=maxit,escape=4.0),
									 cos=cosine_esc(x,y,maxit=maxit,escape=(10*pi)^2),
									 exp=exp_esc(x,y,maxit=maxit,escape=(50)^2))
	}
# Define server logic # FOLDUP
	my_server <- function(input, output, session) {
		viewport <- reactiveValues(xmin=min(init_x),
															 xmax=max(init_x),
															 ymin=min(init_y),
															 ymax=max(init_y))

			 observeEvent(input$zoomout,{
					 xcent <- 0.5 * (viewport$xmin + viewport$xmax)
					 ycent <- 0.5 * (viewport$ymin + viewport$ymax)
					 xdiff <- (viewport$xmax - viewport$xmin)
					 ydiff <- (viewport$ymax - viewport$ymin)

					 uppy <- 1.5
					 viewport$xmin <- xcent - uppy * 0.5 * xdiff
					 viewport$xmax <- xcent + uppy * 0.5 * xdiff
					 viewport$ymin <- ycent - uppy * 0.5 * ydiff
					 viewport$ymax <- ycent + uppy * 0.5 * ydiff
			 })

			 observeEvent(input$panleft,{
					 xdiff <- (viewport$xmax - viewport$xmin)
					 viewport$xmin <- viewport$xmin - 0.33 * xdiff
					 viewport$xmax <- viewport$xmax - 0.33 * xdiff
			 })
			 observeEvent(input$panright,{
					 xdiff <- (viewport$xmax - viewport$xmin)
					 viewport$xmin <- viewport$xmin + 0.33 * xdiff
					 viewport$xmax <- viewport$xmax + 0.33 * xdiff
			 })

			 # When a double-click happens, check if there's a brush on the plot.
			 # If so, zoom to the brush bounds; if not, reset the zoom.
			 observeEvent(input$plot_click, {
				 brush <- input$plot_brush
				 if (!is.null(brush)) {
					 xcent <- 0.5 * (brush$xmin + brush$xmax)
					 ycent <- 0.5 * (brush$ymin + brush$ymax)
					 xdiff <- (brush$xmax - brush$xmin)
					 ydiff <- (brush$ymax - brush$ymin)
					 if (xdiff > ydiff * ar) {
						 viewport$xmin <- brush$xmin
						 viewport$xmax <- brush$xmax
						 viewport$ymin <- ycent - 0.5 * xdiff / ar
						 viewport$ymax <- ycent + 0.5 * xdiff / ar
					 } else {
						 viewport$xmin <- xcent - 0.5 * ar * ydiff
						 viewport$xmax <- xcent + 0.5 * ar * ydiff
						 viewport$ymin <- brush$ymin
						 viewport$ymax <- brush$ymax
					 }
				 } else {
					 viewport$xmin <- min(init_x)
					 viewport$xmax <- max(init_x)
					 viewport$ymin <- min(init_y)
					 viewport$ymax <- max(init_y)
				 }
			 })

			xyzs <- reactive({
				xt <- tibble(x=seq(viewport$xmin,viewport$xmax,length.out=ceiling(5*ar*input$dpi)))
				yt <- tibble(y=seq(viewport$ymin,viewport$ymax,length.out=ceiling(5*input$dpi)))
				crossing(xt,yt) %>% mutate(mi=mandeldeez(x,y,input$resolution,input$fractal))
			})
			output$theplot <- renderPlot({
				ph <- xyzs() %>%
					ggplot(aes(x=x,y=y,fill=log(1+mi))) + 
					scale_fill_viridis(option=input$colorscheme,direction=-1) + 
					geom_tile(size=0) +
					coord_equal() + 
					theme_void() + 
					geom_raster() + 
					guides(color=FALSE,fill=FALSE)
				ph
			},
			height=900,
			width=ceiling(ar*900))
			# should have coords in bookmark. rats.
			setBookmarkExclude(c('bookmark'))
			observeEvent(input$bookmark,{
				session$doBookmark()
			})
	}
	# UNFOLD

	shinyApp(ui=my_ui, server=my_server)
}

options(shiny.reactlog=TRUE)

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
