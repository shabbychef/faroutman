# Copyright 2023 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

# This file is part of faroutman.
#
# faroutman is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# faroutman is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with faroutman.  If not, see <http://www.gnu.org/licenses/>.

# env var:
# nb: 
# see also:
# todo:
# changelog: 
#
# Created: 2023.07.01
# Copyright: Steven E. Pav, 2023-2023
# Author: Steven E. Pav
# Comments: Steven E. Pav

# helpers#FOLDUP
set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}
#UNFOLD

context("code runs at all")#FOLDUP

test_that("escape functions run",{#FOLDUP
	expect_error(mandelbrot_esc(0.25,0.01,maxit=256L),NA)
	expect_error(cosine_esc(0.25,0.01,maxit=256L),NA)
	expect_error(exp_esc(0.25,0.01,maxit=256L),NA)
	expect_error(fibonacci_esc(0.25,0.01,maxit=256L),NA)
	expect_error(burning_ship_esc(0.25,0.01,maxit=256L),NA)
})#UNFOLD
#UNFOLD


#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
