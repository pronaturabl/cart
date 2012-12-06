# cart

diffusion-based cartograms with R

Cart produces density-equalising maps or "cartograms" based on a
diffusion-based method by Michael T. Gastner and Mark E. J. Newman.

## Dependencies

[`Cart`](http://www-personal.umich.edu/~mejn/cart/), the cartogram software by
Michael T. Gastner and Mark E. J. Newman, consists of a couple of C source code
files that are shipped with the R package. `Cart` depends on the
[`FFTW`](http://www.fftw.org/) C library, which can be installed with

    apt-get install libfftw3-dev

on Debian and with

    zypper in fftw3-devel

on openSUSE.
