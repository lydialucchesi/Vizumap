# Vizumap JOSS paper revisions

### README 
* Edited method descriptions
* Hyperlinked to open-access papers
* Fixed license description
* Switched from devtools to remotes installation
* Removed Zenodo reference
* Added section about contributing to the package
  * Generated a Code of Conduct file with usethis::use_code_of_conduct()
  * Added contribution guidelines file

### License
* Removed incorrect file from repo
* Added correct full text license file (GPLv3)

### Description
* Corrected package version (it should be v1.1.0)
  * In the JOSS submission form, it was submitted as v1.2.0 - is this going to be an issue?
* Deleted extra maintainer
* Added a longer description
* Updated author fields and included contribution descriptions
* Added ORCIDs for all authors

### Vignette
* Improved variable/item naming
* Fixed Mexico border overlap
* Added all() wrapper
* Added California border to pixel map to improve map interpretability
* Included running of pixelation code (seems fast enough to include)

### Vizumap code
* Added \dontrun{} to examples instead of commenting out
* Renamed "shapefile" argument to "geoData"
* Reformatted comments to follow Hadley style guide ("# comment" instead of "#comment")

### Paper
* Moved the directory to Rbuildignore
* Included an R script with code that produces the paper figures
* Added funding acknowledgement
* Incorporated Daniel’s rephrasing suggestions
  * “finding methods that add additional elements” to “finding methods that can communicate additional information, about the spatial estimates, in an understandable and meaningful way”
  * “functions can be found in the package download” to “is available after package installation”
* Clarified audience for statement of need requirement:
  * “However, in spatial applications, finding methods that can communicate additional information, about the spatial estimates, in an understandable and meaningful way can be challenging. To address this visualisation shortcoming in spatial statistics, we developed the Vizumap R package. **It is a toolkit designed for statisticians, scientists, data journalists, etc., discussing uncertainty in spatial data.**”






