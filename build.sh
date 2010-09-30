#!/bin/sh

# roxygenize
R CMD roxygen pkg

# check
R CMD check pkg

# build tar ball
R CMD build pkg

# install from tar ball
R CMD INSTALL cart_*.tar.gz

