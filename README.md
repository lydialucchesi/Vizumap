
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Vizumap <img src='man/figures/Vizumap_Hex.png' align="right" height="138.5" />

## An R package for visualizing uncertainty in spatial data.

## Installation

You can install a development version of the Vizumap package from
[GitHub](https://github.com/lydialucchesi/Vizumap)

    # install.packages("devtools")
    devtools::install_github(repo = "lydialucchesi/Vizumap", build_vignettes = TRUE, force = TRUE)

## Authors

Lydia Lucchesi, Australian National University & CSIRO Data61, Email:
<Lydia.Lucchesi@anu.edu.au>

Petra Kuhnert, CSIRO Data61, Email: <Petra.Kuhnert@data61.csiro.au>

## About the Package

Approaches for visualizing uncertainty in spatial data are presented in
this package. These include the three approaches developed in Lucchesi
and Wikle (2017) and a fourth approach presented in Kuhnert et
al. (2018) that uses exceedance probabilities to convey uncertainty on
a map (or stream network) from a spatio-temporal model.

### Bivariate Maps

This approach is an extension of previous implementations of choropleth
mapping that includes new features to make the maps more useful for
interpretation. Bivariate choropleth maps explore the “blending” of two
colour schemes, one representing the estimate and a second representing
the margin of error, which can be conveyed on a map through a single
blended colour. Bivariate colour maps can be created for both areal and
point level data. Using a bivariate colour grid, estimates from a
spatial model (e.g. mean) and the uncertainty surrounding these values
(e.g. standard deviation) are mapped simulataneously. Vizumap allows the
user to create a bivariate colour scheme by mathematically blending two
single hue colour palettes and organising them on a 3 x 3 grid. Users
can develop their own bivariate colour palette as well as select from
some pre-prepared palettes.

### Map Pixelation

This approach uses map pixelation to convey uncertainty. This is a novel
approach that pixelates areas within each region of the map according to
the size of the margin of error. Regions that appear visually as a solid
piece of colour reflect smaller margins of error, while more pixelated
regions indicate larger margins of error. These maps can be animated
using visuanimation techniques to provide a novel “user experience” to
visualizing uncertainty on a map.

### Glyph Rotation

Glyph rotation uses a glyph to represent uncertainty. When rotated in
different ways, the rotation of the glyph corresponds to the margin of
error.

### Exceedance Probability Maps

The final map based exploration of uncertainty is through exceedance
probabilities, that can be showcased on a map to highlight regions that
exhibit varying levels of departure from a threshold of concern or
target.

## Examples

A vignette for the Vizumap package is available and contains examples
relating to each of the visualization methods.

    vignette("Vizumap")

## How to Reference

Vizumap: An R package for visualizing uncertainty in spatial data, DOI:
10.5281/zenodo.1013930, <https://zenodo.org/record/1479951#.Xnr_Qy1L2L8>

## License

The package Vizumap version 1.1.0 is licensed under GPL-3 (see LICENSE
file)

## References

<strong>Kuhnert, P.M.</strong>, Pagendam, D.E., Bartley, R., Gladish,
D.W., Lewis, S.E. and Bainbridge, Z.T. (2018) Making management
decisions in face of uncertainty: a case study using the Burdekin
catchment in the Great Barrier Reef, Marine and Freshwater Research, 69,
1187-1200, <https://doi.org/10.1071/MF17237>.

<strong>Lucchesi, L.R.</strong> and Wikle C.K. (2017) Visualizing
uncertainty in areal data with bivariate choropleth maps, map pixelation
and glyph rotation, Stat, <https://doi.org/10.1002/sta4.150>.
