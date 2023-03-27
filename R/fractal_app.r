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
#' @importFrom shinythemes shinytheme
#'
#' @export
fractal_app <- function() {
# 2FIX: parametrize these
	ar <- 1.667
	ywid <- 2.5
	xwid <- ywid * ar
	init_x <<- -0.5 + (xwid/2) * c(-1,1)
	init_y <<- (ywid/2) * c(-1,1)
	cent_x <- mean(init_x)
	cent_y <- mean(init_y)

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
														 numericInput("center_x","x center: ",min=-2,max=2,value=cent_x,width='80%'),
														 numericInput("center_y","y center: ",min=-2,max=2,value=cent_y,width='80%'),
														 numericInput("width_x","x width: ",min=0,max=4,value=xwid,width='80%'),
														 numericInput("width_y","y width: ",min=0,max=4,value=ywid,width='80%'),
														 actionButton("go","go"),
														 hr(),
														 actionButton("zoomout","zoom out"),
														 actionButton("zoomin","zoom in"),
														 actionButton("panleft","pan left"),
														 actionButton("panright","pan right"),
														 hr(),
														 sliderInput("resolution","Depth: ",min=10,max=9000,value=95,step=5),
														 sliderInput("dpi", "Pixel Resolution: ",min=50,max=200,value=100,step=10),
														 hr(),
														 selectInput("fractal","Fractal: ",
																				 choices=c('Mandelbroit','Fibonacci','Cos','Exp','Burning Ship'),
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

	mandeldeez <- function(x,y,maxit=100,fractal=c('Mandelbroit','Fibonacci','Cos','Exp','Burning Ship')) {
		fractal <- match.arg(fractal)
		retv <- switch(gsub('\\s','_',tolower(fractal)),
									 mandelbroit=mandelbrot_esc(x,y,maxit=maxit,escape=4.0),
									 fibonacci=fibonacci_esc(x,y,maxit=maxit,escape=4.0),
									 cos=cosine_esc(x,y,maxit=maxit,escape=(10*pi)^2),
									 exp=exp_esc(x,y,maxit=maxit,escape=(50)^2),
									 burning_ship=burning_ship_esc(x,y,maxit=maxit,escape=4.0))
	}


# set the viewport
	set_vp <- function(viewport,xcent,ycent,xdiff,ydiff,go_small=FALSE) {
		if (go_small) {
			nexd <- min(xdiff,ydiff*ar)
			neyd <- min(ydiff,xdiff/ar)
		} else {
			nexd <- max(xdiff,ydiff*ar)
			neyd <- max(ydiff,xdiff/ar)
		}
		xdiff <- nexd
		ydiff <- neyd

		viewport$xmin <- xcent - 0.5 * xdiff
		viewport$xmax <- xcent + 0.5 * xdiff
		viewport$ymin <- ycent - 0.5 * ydiff
		viewport$ymax <- ycent + 0.5 * ydiff
		viewport$xdel <- xdiff
		viewport$ydel <- ydiff
		return(viewport)
	}

# Define server logic # FOLDUP
	my_server <- function(input, output, session) {
		viewport <- reactiveValues(xmin=min(init_x),
															 xmax=max(init_x),
															 ymin=min(init_y),
															 ymax=max(init_y),
															 xdel=max(init_x) - min(init_x),
															 ydel=max(init_y) - min(init_y))

			 observeEvent({
				 #input$center_x
				 #input$center_y
				 input$go
			 },{
					 xcent <- input$center_x
					 ycent <- input$center_y
					 xdiff <- input$width_x
					 ydiff <- input$width_y
					 viewport <- set_vp(viewport,xcent,ycent,xdiff,ydiff,go_small=TRUE)
					 updateNumericInput(session,'center_x',value=xcent)
					 updateNumericInput(session,'center_y',value=ycent)
					 updateNumericInput(session,'width_x',value=viewport$xdel)
					 updateNumericInput(session,'width_y',value=viewport$ydel)
			 })


			 observeEvent(input$zoomout,{
											uppy <- 1.5
											xcent <- 0.5 * (viewport$xmin + viewport$xmax)
											ycent <- 0.5 * (viewport$ymin + viewport$ymax)
											xdiff <- uppy * (viewport$xmax - viewport$xmin)
											ydiff <- uppy * (viewport$ymax - viewport$ymin)

											viewport <- set_vp(viewport,xcent,ycent,xdiff,ydiff)
											updateNumericInput(session,'center_x',value=xcent)
											updateNumericInput(session,'center_y',value=ycent)
											updateNumericInput(session,'width_x',value=viewport$xdel)
											updateNumericInput(session,'width_y',value=viewport$ydel)
			 })
			 observeEvent(input$zoomin,{
											uppy <- 0.666
											xcent <- 0.5 * (viewport$xmin + viewport$xmax)
											ycent <- 0.5 * (viewport$ymin + viewport$ymax)
											xdiff <- uppy * (viewport$xmax - viewport$xmin)
											ydiff <- uppy * (viewport$ymax - viewport$ymin)

											viewport <- set_vp(viewport,xcent,ycent,xdiff,ydiff)
											updateNumericInput(session,'center_x',value=xcent)
											updateNumericInput(session,'center_y',value=ycent)
											updateNumericInput(session,'width_x',value=viewport$xdel)
											updateNumericInput(session,'width_y',value=viewport$ydel)
			 })

			 observeEvent(input$panleft,{
											xcent <- 0.5 * (viewport$xmin + viewport$xmax)
											ycent <- 0.5 * (viewport$ymin + viewport$ymax)
											xdiff <- (viewport$xmax - viewport$xmin)
											ydiff <- (viewport$ymax - viewport$ymin)
											xcent <- xcent - 0.33 * xdiff
											viewport <- set_vp(viewport,xcent,ycent,xdiff,ydiff)
											updateNumericInput(session,'center_x',value=xcent)
											updateNumericInput(session,'center_y',value=ycent)
											updateNumericInput(session,'width_x',value=viewport$xdel)
											updateNumericInput(session,'width_y',value=viewport$ydel)
			 })
			 observeEvent(input$panright,{
											xcent <- 0.5 * (viewport$xmin + viewport$xmax)
											ycent <- 0.5 * (viewport$ymin + viewport$ymax)
											xdiff <- (viewport$xmax - viewport$xmin)
											ydiff <- (viewport$ymax - viewport$ymin)
											xcent <- xcent + 0.33 * xdiff
											viewport <- set_vp(viewport,xcent,ycent,xdiff,ydiff)
											updateNumericInput(session,'center_x',value=xcent)
											updateNumericInput(session,'center_y',value=ycent)
											updateNumericInput(session,'width_x',value=viewport$xdel)
											updateNumericInput(session,'width_y',value=viewport$ydel)
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
				 } else {
					 xcent <- input$center_x
					 ycent <- input$center_y
					 xdiff <- (max(init_x) - min(init_x))
					 ydiff <- (max(init_y) - min(init_y))
				 }
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
				 viewport$xdel <- xdiff
				 viewport$ydel <- ydiff
				 updateNumericInput(session,'center_x',value=xcent)
				 updateNumericInput(session,'center_y',value=ycent)
				 updateNumericInput(session,'width_x',value=xdiff)
				 updateNumericInput(session,'width_y',value=ydiff)
			 })

			xyzs <- reactive({
				xt <- tibble(x=seq(viewport$xmin,viewport$xmax,length.out=ceiling(5.2*ar*input$dpi)))
				yt <- tibble(y=seq(viewport$ymin,viewport$ymax,length.out=ceiling(5.2*input$dpi)))
				crossing(xt,yt) %>% mutate(mi=mandeldeez(x,y,input$resolution,input$fractal))
			})
			output$theplot <- renderPlot({
				ph <- xyzs() %>%
					#mutate(transfill=sqrt(mi-min(mi))) %>%
					mutate(transfill=log(1+mi-min(mi))) %>%
					ggplot(aes(x=x,y=y,fill=transfill)) + 
					scale_fill_viridis(option=input$colorscheme,direction=-1) + 
					geom_tile(size=0) +
					coord_equal() + 
					theme_void() + 
					geom_raster() + 
					guides(color='none',fill='none')
				ph
			},
			height=850,
			width=ceiling(ar*850))
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
