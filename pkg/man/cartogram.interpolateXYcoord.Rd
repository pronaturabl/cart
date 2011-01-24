\name{cartogram.interpolateXYcoord}
\alias{cartogram.interpolateXYcoord}
\title{Performs bilinear interpolation.}
\usage{cartogram.interpolateXYcoord(xy, Cartogram)}
\description{Performs bilinear interpolation.}
\details{This function performs bilinear interpolation of pairs of x/y
coordinates based on a matrix of x gridcoordinates and a matrix of y
grid coordinates. The algorithm is based on C code by Mark Newman.}
\author{Thomas Zumbrunn \email{thomas@zumbrunn.name}}
\value{matrix of x/y transformed coordinates}
\arguments{\item{xy}{numeric matrix of x and y coordinate pairs to be transformed}
\item{xgrid}{numeric matrix of x grid coordinates}
\item{ygrid}{numeric matrix of y grid coordinates}}
