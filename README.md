

# faroutman

[![Build Status](https://github.com/shabbychef/faroutman/workflows/R-CMD-check/badge.svg)](https://github.com/shabbychef/faroutman/actions)


A Shiny app to look at fractals.

-- Steven E. Pav, shabbychef@gmail.com

## Installation

This package can be installed 
via [drat](https://github.com/eddelbuettel/drat "drat"), or 
from [github](https://github.com/shabbychef/faroutman "faroutman")
via devtools:


```r
# via drat:
if (require(drat)) {
    drat:::add("shabbychef")
    install.packages("faroutman")
}
# get snapshot from github (may be buggy)
if (require(devtools)) {
	# latest greatest
	install_github('shabbychef/faroutman',ref='master')
}
```

# Basic Usage

Run the app via the `faroutman::fractal_app()` function.  

One can zoom in the fractal window, then double click to reset the pane.
Navigation via the x and y center and width is also possible.
Four fractal types are supported: Mandelbrot, Fibonacci, Cosine and
Exponential.


## Screenshots

![](man/figures/screenshot_mandelbrot_one.png)
![](man/figures/screenshot_mandelbrot_two.png)
![](man/figures/screenshot_cos_one.png)



