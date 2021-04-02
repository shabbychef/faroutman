/*

  This file is part of faroutman.
  
  faroutman is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
  
  faroutman is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public License
  along with faroutman.  If not, see <http://www.gnu.org/licenses/>.


  Created: 2021-04-01
  Copyright: Steven E. Pav, 2021
  Author: Steven E. Pav <shabbychef@gmail.com>
  Comments: Steven E. Pav
*/

#ifndef __DEF_faroutman__
#define __DEF_faroutman__

#endif /* __DEF_faroutman__ */

#include <Rcpp.h>
using namespace Rcpp;

// mandelbrot escape iteration function//FOLDUP

//' @title
//' Mandelbrot escape function
//' @description
//' Compute the Mandelbrot set.
//'
//' @details
//' Computes the iterations required to escape based on
//' \eqn{z_n \leftarrow z_{n-1}^2 + c}
//' given input \eqn{c}.
//'
//' @param x  the real coordinates
//' @param y  the imaginary coordinates
//' @param maxit  the maximum iterations to consider
//' @param escape   the condition to determine escape, in squared distance units.
//' @template etc
//' @name fractals
//' @rdname fractals
//' @export
// [[Rcpp::export]]
IntegerVector mandelbrot_esc(NumericVector x, NumericVector y,
                         int maxit=128,double escape=4.0) {
  if (x.size() != y.size()) { stop("real and imaginary parts should have same size"); }
  IntegerVector retv(x.size());

  // how long for z^2 + c to escape
  int it;
  double x2,y2,modulus;
  double myx,myy;

  int iii;
  for (iii=0;iii < x.size();iii++) {
      it = 0;
      myx = 0;myy = 0;
      while (it < maxit) {
        x2 = myx*myx;
        y2 = myy*myy;
        modulus = x2 + y2;
        if (modulus > escape) { 
            break;
        }
        it++;
        myy = 2 * myx * myy + y[iii];
        myx = x2 - y2 + x[iii];
      }
      retv[iii] = it;
  }
  return(retv);
}
//UNFOLD
// fibonacci escape iteration function//FOLDUP

//' @title
//' Fibonacci escape function
//' @description
//' Compute the Fibonacci set.
//'
//' @details
//' Computes the iterations required to escape based on
//' \eqn{z_n \leftarrow z_{n-1}^2 + z_{n-2} + c}
//' given input \eqn{c}.
//'
//' @seealso \url{https://math.stackexchange.com/a/5705}
//'
//' @param x  the real coordinates
//' @param y  the imaginary coordinates
//' @param maxit  the maximum iterations to consider
//' @param escape   the condition to determine escape, in squared distance units.
//' @template etc
//' @name fractals
//' @rdname fractals
//' @export
// [[Rcpp::export]]
IntegerVector fibonacci_esc(NumericVector x, NumericVector y,
                        int maxit=128,double escape=4.0) {
  if (x.size() != y.size()) { stop("real and imaginary parts should have same size"); }
  IntegerVector retv(x.size());

  // how long for z^2 + zprev + c to escape
  // from Americo Tavares
  // https://math.stackexchange.com/a/5705
  int it;
  double x2,y2,modulus;
  double myx,myy;
  double prx,pry;
  double tmprx,tmpry;

  int iii;
  for (iii=0;iii < x.size();iii++) {
      it = 0;
      myx=0;myy=0;
      prx=0;pry=0;
      while (it < maxit) {
        x2 = myx*myx;
        y2 = myy*myy;
        modulus = x2 + y2;
        if (modulus > escape) { 
            break;
        }
        it++;
        tmprx = prx;tmpry = pry;
        prx = myx;pry = myy;
        myy = 2 * myx * myy + tmpry + y[iii];
        myx = x2 - y2 + tmprx + x[iii];
      }
      retv[iii] = it;
  }
  return(retv);
}
//UNFOLD
// cosine escape iteration function//FOLDUP

//' @title
//' Cosine escape function
//' @description
//' Compute the Cosine set.
//'
//' @details
//' Computes the iterations required to escape based on
//' \eqn{z_n \leftarrow cos(z_{n-1}) + c}
//' given input \eqn{c}.
//'
//' @param x  the real coordinates
//' @param y  the imaginary coordinates
//' @param maxit  the maximum iterations to consider
//' @param escape   the condition to determine escape, in squared distance units.
//' @template etc
//' @name fractals
//' @rdname fractals
//' @export
// [[Rcpp::export]]
IntegerVector cosine_esc(NumericVector x, NumericVector y,
                     int maxit=128,double escape=987.0) {
  if (x.size() != y.size()) { stop("real and imaginary parts should have same size"); }
  IntegerVector retv(x.size());

  // how long for cos(z) + c to escape
  int it;
  double x2,y2,modulus;
  double myx,myy,tmpx;

  int iii;
  for (iii=0;iii < x.size();iii++) {
    myx=0;myy=0;
    while (it < maxit) {
        x2 = myx*myx;
        y2 = myy*myy;
        modulus = x2 + y2;
        if (modulus > escape) { break; }
        it++;
        tmpx = myx;
        myx = cos(myx)*cosh(myy) + x[iii];
        myy = -sin(tmpx)*sinh(myy) + y[iii];
    }
    retv[iii] = it;
  }
  return(retv);
}
//UNFOLD
// exp escape iteration function//FOLDUP

//' @title
//' Exp escape function
//' @description
//' Compute the Exp set.
//'
//' @details
//' Computes the iterations required to escape based on
//' \eqn{z_n \leftarrow exp(z_{n-1}) + c}
//' given input \eqn{c}.
//'
//' @param x  the real coordinates
//' @param y  the imaginary coordinates
//' @param maxit  the maximum iterations to consider
//' @param escape   the condition to determine escape, in squared distance units.
//' @template etc
//' @name fractals
//' @rdname fractals
//' @export
// [[Rcpp::export]]
IntegerVector exp_esc(NumericVector x, NumericVector y,
                  int maxit=128,double escape=2500.0) {
  if (x.size() != y.size()) { stop("real and imaginary parts should have same size"); }
  IntegerVector retv(x.size());

  // how long for exp(z) + c to escape
  int it;
  double x2,y2,modulus;
  double myx,myy;
  double emx;

  int iii;
  for (iii=0;iii < x.size();iii++) {
    myx=0;myy=0;
    while (it < maxit) {
        x2 = myx*myx;
        y2 = myy*myy;
        modulus = x2 + y2;
        if (modulus > escape) { break; }
        it++;
		emx = exp(myx);
        myx = emx*cos(myy) + x[iii];
        myy = emx*sin(myy) + y[iii];
    }
    retv[iii] = it;
  }
  return(retv);
}
//UNFOLD

//for vim modeline: (do not edit)
// vim:et:nowrap:ts=4:sw=4:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=//%s:tags=.c_tags;:syn=cpp:ft=cpp:mps+=<\:>:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
