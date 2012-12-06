# cart

diffusion-based cartograms with R

Cart produces density-equalising maps or "cartograms" based on a
diffusion-based method by Michael T. Gastner and Mark E. J. Newman.

## Dependencies

[`Cart`](http://www-personal.umich.edu/~mejn/cart/), the cartogram software by
Michael T. Gastner and Mark E. J. Newman, consists of a couple of C source code
files that are shipped with the R
package. [`Cart`](http://www-personal.umich.edu/~mejn/cart/)
depends on the [`FFTW`](http://www.fftw.org/) C library, which can be
installed on Debian with

    apt-get install libfftw3-dev

on Fedora with

    yum install fftw3-devel

and on openSUSE with

    zypper install fftw3-devel


