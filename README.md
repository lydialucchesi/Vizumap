
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Vizumap <img src='man/figures/Vizumap_Hex.png' align="right" height="138.5" />

## An R package for visualizing uncertainty in spatial data.

[![R build
status](https://github.com/lydialucchesi/Vizumap/workflows/R-CMD-check/badge.svg)](https://github.com/lydialucchesi/Vizumap/actions)

`Vizumap` pkgdown site with vignette:
<https://lydialucchesi.github.io/Vizumap/>.

## Installation

You can install a development version of the `Vizumap` package using the
command below.

    remotes::install_github(repo = "lydialucchesi/Vizumap", build_vignettes = TRUE, force = TRUE)

## Authors

Lydia Lucchesi, Australian National University & CSIRO Data61, Email:
<Lydia.Lucchesi@anu.edu.au>

Petra Kuhnert, CSIRO Data61, Email: <Petra.Kuhnert@data61.csiro.au>

## About the package

Approaches for visualising uncertainty in spatial data are presented in
this package. These include the three approaches developed in [Lucchesi
and Wikle
(2017)](http://faculty.missouri.edu/~wiklec/LucchesiWikle2017Stat) and a
fourth approach presented in [Kuhnert et
al. (2018)](https://publications.csiro.au/publications/#publication/PIcsiro:EP168206).

#### Bivariate Maps

In these bivariate choropleth maps, two colour schemes, one representing
the estimates and another representing the margins of error, are blended
so that an estimate and its error can be conveyed on a map using a
single colour.

#### Map Pixelation

In this approach, each map region is pixelated. Pixels are filled with
colours representing values within an estimate’s margin of error.
Regions that appear as a solid colour reflect smaller margins of error,
while more pixelated regions indicate greater uncertainty. These maps
can be animated to provide a novel uncertainty visualisation experience.

#### Glyph Rotation

In this method, glyphs located at region centroids are rotated to
represent uncertainty. The colour filling each glyph corresponds to the
estimate.

#### Exceedance Probability Maps

The final map-based exploration is through exceedance probabilities,
which are visualised on a map to highlight regions that exhibit varying
levels of departure from a threshold of concern or target.

## Examples

A for the `Vizumap` package is available and contains examples relating
to each of the visualisation methods.

    vignette("Vizumap")

## Testing

If you would like to install and run the unit tests interactively,
include `INSTALL_opts = "--install-tests"` in the installation code.

    remotes::install_github(repo = "lydialucchesi/Vizumap", build_vignettes = TRUE, force = TRUE, INSTALL_opts = "--install-tests")
    
    testthat::test_package("Vizumap", reporter = "stop")

## Contribute

To contribute to `Vizumap`, please follow these
[guidelines](CONTRIBUTING.md).

Please note that the `Vizumap` project is released with a [Contributor
Code of Conduct](CONDUCT.md). By contributing to this project, you agree
to abide by its terms.

## License

`Vizumap` version 1.2.0 is licensed under [GPLv3](LICENSE.md).

## History of Vizumap

Vizumap began as a visualisation project at the University of Missouri
in 2016. Chris Wikle, professor of statistics, posed an interesting
research question to Lydia Lucchesi, a student curious about data
visualisation and R.

How do you include uncertainty on a map displaying areal data estimates?

Over the course of a year, they put together three methods for
visualising uncertainty in spatial statistics: the bivariate choropleth
map, the pixel map, and the glyph map. By mid-2017, there were maps, and
there was a lot of R code, but there was not a tool that others could
use to easily make these types of maps, too. That’s when statistician
Petra Kuhnert recommended developing an R package. Over the course of a
month, Petra and Lydia developed Vizumap (originally named VizU) at
CSIRO Data61 in Canberra, Australia. Since then, the package has been
expanded to include exceedance probability maps, an uncertainty
visualisation method developed by Petra while working on a Great Barrier
Reef (GBR) project.

Vizumap has been used to visualise the uncertainty of American Community
Survey estimates, the prediction errors of sediment estimates in a GBR
catchment, and most recently the [uncertainty of estimated locust
densities in
Australia](https://www.nature.com/articles/s41598-020-73897-1/figures/4).
We would like to assemble a Vizumap gallery that showcases different
applications of the package’s mapping methods. If you use Vizumap to
visualise uncertainty, please feel free to send the map our way. We
would like to see it\!

## References

Kuhnert, P.M., Pagendam, D.E., Bartley, R., Gladish, D.W., Lewis, S.E.
and Bainbridge, Z.T. (2018) [Making management decisions in face of
uncertainty: a case study using the Burdekin catchment in the Great
Barrier Reef, Marine and Freshwater
Research](https://publications.csiro.au/publications/#publication/PIcsiro:EP168206),
69, 1187-1200, <https://doi.org/10.1071/MF17237>.

Lucchesi, L.R. and Wikle C.K. (2017) [Visualizing uncertainty in areal
data with bivariate choropleth maps, map pixelation and glyph
rotation](http://faculty.missouri.edu/~wiklec/LucchesiWikle2017Stat),
Stat, <https://doi.org/10.1002/sta4.150>.
