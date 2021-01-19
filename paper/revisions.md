# Vizumap JOSS paper revisions

(DN) - Edit based on revision suggested by Daniel Nüst (https://github.com/openjournals/joss-reviews/issues/2409#issuecomment-664007471)

(SD) - Edit based on revision suggested by Shaoqing Dai (https://github.com/openjournals/joss-reviews/issues/2409#issuecomment-736095413)

### README 
* Edited method descriptions
* (DN) Hyperlinked to open-access papers
* (DN) Fixed license description
* (DN) Switched from devtools to remotes installation
* (DN) Removed Zenodo reference
* (DN) Added section about contributing to the package
  * Generated a Code of Conduct file with usethis::use_code_of_conduct()
  * Added contribution guidelines file
* (DN) Added section about running tests
* (DN) Added action badge
* (DN & editor @bstabler) Added section about Vizumap collaboration history
* (SD) Added links to pkgdown site and vignette

### Website
* (SD) Generated a website for Vizumap using pkgdown

### License
* (DN) Removed incorrect file from repo
* (DN) Added correct full text license file (GPLv3)

### Description
* (DN) Corrected package version (it should be v1.0.0)
  * In the JOSS submission form, it was submitted as v1.2.0 - is this going to be an issue?
  * UPDATED PACKAGE VERSION TO 1.1.0
* (DN) Deleted extra maintainer
* (DN) Added a longer description
* (DN & editor @bstabler) Updated author fields
* (DN) Added ORCIDs for all authors
* Added pkgdown and source code urls

### Vignette
* (DN) Added exceedance map example
* (DN) Improved variable/item naming
* (DN) Fixed Mexico border overlap
* (DN) Added all() wrapper
* Added California border to pixel map to improve map interpretability
* Included running of pixelation code (seems fast enough to include)

### Vizumap code
* (DN) Added `\dontrun{}` to examples instead of commenting out
* (DN) Renamed "shapefile" argument to "geoData"
* Reformatted comments to follow Hadley style guide ("# comment" instead of "#comment")
* (DN) Started adding testing for data formatting
* (DN and SD) Addressed pixelate warnings (remove and then return to original projection)
* (DN) Added comment to pixelate documentation about projection changes

### Paper
* (DN) Moved the directory to Rbuildignore
* (DN) Included an R script with code that produces the paper figures
* Added funding acknowledgement for second author
* (DN) Incorporated Daniel’s rephrasing suggestions
  * “finding methods that add additional elements” to “finding methods that can communicate additional information, about the spatial estimates, in an understandable and meaningful way”
  * “functions can be found in the package download” to “is available after package installation”
* (DN) Clarified audience for statement of need requirement:
  * “However, in spatial applications, finding methods that can communicate additional information, about the spatial estimates, in an understandable and meaningful way can be challenging. To address this visualisation shortcoming in spatial statistics, we developed the Vizumap R package. **It is a toolkit designed for statisticians, scientists, data journalists, etc., discussing uncertainty in spatial data.**”
* (DN) Added zoom view of five regions for the pixel map figure and a sentence about this in the map description
* (DN) Commented on related R packages
* Added testthat and usethis to package acknowledgements
* (DN) Fixed text under pixel figure
* (SD) Added figure captions
* (SD) Numbered equations
* (SD) Added sub-figure labels to pixel map
* Changed "TSS loads" to "TSS concentrations"
* Edits to pixel sampling description







