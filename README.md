
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Vizumap <img src='man/figures/Vizumap_Hex.png' align="right" height="138.5" />

## An R package for visualizing uncertainty in spatial data.

## Installation

You can install a development version of the `Vizumap` package using the
command below.

    remotes::install_github(repo = "lydialucchesi/Vizumap", build_vignettes = TRUE, force = TRUE)

## Authors

Lydia Lucchesi, Australian National University & CSIRO Data61, Email:
<Lydia.Lucchesi@anu.edu.au>

Petra Kuhnert, CSIRO Data61, Email: <Petra.Kuhnert@data61.csiro.au>

## About the Package

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

A vignette for the `Vizumap` package is available and contains examples
relating to each of the visualisation methods.

    vignette("Vizumap")

## Contribute

Please note that the `Vizumap` project is released with a [Contributor
Code of Conduct](CONDUCT.md). By contributing to this project, you agree
to abide by its terms.

To contribute to `Vizumap`, please follow these
[guidelines](CONTRIBUTING.md).

## License

`Vizumap` version 1.1.0 is licensed under [GPLv3](LICENSE.md).

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
