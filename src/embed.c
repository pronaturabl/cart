/* Example program to calculate a grid of points for a Gastner-Newman
 * cartogram using the cartogram.c code
 *
 * Written by Mark Newman
 *
 * See http://www.umich.edu/~mejn/ for further details.
 */


#include <stdio.h>
#include <stdlib.h>

#include "cart.h"


/* The parameter OFFSET specifies a small amount to be added the density in
 * every grid square, as a Fraction of the mean density on the whole
 * lattice.  This prevents negative densities from being generated by
 * numerical errors in the FFTs which can cause problems for the
 * integrator.  If the program is giving weird behavior, particularly at
 * the beginning of a calculation, try increasing this quantity by a factor
 * of 10.
 */

#define OFFSET 0.005


/* Function to read population data into the array rho.  Returns 1 if there
 * was a problem, zero otherwise */

int embed_readpop(double *dens, double **rho, int xsize, int ysize)
{
  int ix,iy;
  double mean;
  double sum=0.0;

  for (iy=0; iy<ysize; iy++) {
    for (ix=0; ix<xsize; ix++) {
      rho[ix][iy] = dens[iy * xsize + ix];
      sum += rho[ix][iy];
    }
  }

  mean = sum/(xsize*ysize);
  for (iy=0; iy<ysize; iy++) {
    for (ix=0; ix<xsize; ix++) {
      rho[ix][iy] += OFFSET*mean;
    }
  }

  return 0;
}


/* Function to make the grid of points */

void embed_creategrid(double *gridx, double *gridy, int xsize, int ysize)
{
  int ix,iy;
  int i;

  for (iy=0,i=0; iy<=ysize; iy++) {
    for (ix=0; ix<=xsize; ix++) {
      gridx[i] = ix;
      gridy[i] = iy;
      i++;
    }
  }
}


void embed_main(int xsize, int ysize, double *dens, double *gridx, double *gridy)
{

  double **rho;          // Initial population density

  /* Allocate space for the cartogram code to use */

  cart_makews(xsize,ysize);

  /* Read in the population data, transform it, then destroy it again */

  rho = cart_dmalloc(xsize,ysize);
  embed_readpop(dens, rho, xsize, ysize);
  cart_transform(rho,xsize,ysize);
  cart_dfree(rho);

  /* Create the grid of points */

  embed_creategrid(gridx,gridy,xsize,ysize);

  /* Make the cartogram */

  cart_makecart(gridx,gridy,(xsize+1)*(ysize+1),xsize,ysize,0.0);

  /* Free up the allocated space */

  cart_freews(xsize,ysize);

}
