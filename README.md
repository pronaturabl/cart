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

## Installation

After checking out a copy of the source code, build the R package tarball with:

    R CMD build cart/

Then, install it with:

    R CMD INSTALL cart_x.y.z.tar.gz

If you want to install the R package tarball and get an error message such as

    Error in untar2(tarfile, files, list, exdir) : unsupported entry type ‘x’

this means the R internal function `untar2` fails. In this case, set the
environment variable `R_INSTALL_TAR` to a system `tar` utility, e.g., on
GNU/Linux, do

    export R_INSTALL_TAR="tar"

