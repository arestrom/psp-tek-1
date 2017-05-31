# Effectiveness and Evaluation Tool
Tim Blankemeyer, Emma Clarke, and Katrina Gertz are a group of MLIS Candidates from the University of Washington's Information School who are interested in leveraging open data and open-source tools to help solve complex problems.

Numerous environmental restoration projects have been undertaken throughout the Puget Sound, but **connecting investments in these projects to co-located indicators of habitat viability** is challenging. For this [Open Data Literacy (ODL) Capstone project](https://ischool.uw.edu/capstone/projects/2017/opening-data-visualizing-effectiveness-puget-sound-restoration-efforts), we've collaborated with the Puget Sound Partnership, the Governor's Salmon Recovery Office, South Sound Spatial, and other partners to design a scalable data cleaning and analysis pipeline as well as an interactive visualization prototype to show what's working to restore Puget Sound. 

**CAVEAT:** This is a prototype to show what a web-based analysis tool *might* look like. The underlying data for water quality and salmon were sourced from public web sites. Data and results have not been vetted or approved; that is the next step.

See the prototype at https://ejclarke.shinyapps.io/capstone/.

## Contact
Any questions or comments can be directed to Leska Fore from the Puget Sound Partnership at leska.fore@psp.wa.gov.

## Table of Contents

- [Data Sources](#data-sources)
    + [Project investment Data:](#project-investment-data-)
    + [Water Quality Data:](#water-quality-data-)
    + [Hood Canal Summer Chum Salmon Data:](#hood-canal-summer-chum-salmon-data-)
- [Project tools](#project-tools)
- [Usage](#usage)
- [Key terminology](#key-terminology)
- [xxxxx](#xxxxx)
- [Acknowledgments](#acknowledgments)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>

## Data Sources
As mentioned in the Description, the underlying data for the prototype project were sourced from public web sites.

#### Project investment Data:

* [Ecology's Administration of Grants and Loans (EAGL) Investment Projects](http://www.ecy.wa.gov/funding/EAGL.html):
	* Note: Access to data requires log-in
* [PRoject Information SysteM (PRISM) Investment Projects](http://www.rco.wa.gov/prism_app/about_prism.shtml)
	* Note: Access to data requires log-in

#### Water Quality Data:

* [Washington State Department of Ecology's Environmental Information Management (EIM)](http://www.ecy.wa.gov/eim/)
	* Search for data limited to result parameters Turbidity and TSS, and WRIA's Kennedy-Goldsborough (14), Kitsap (15), Skokomish-Dosewallips (16), Quilcence-Snow (17), and Elwah-Dungeness (18).

#### Hood Canal Summer Chum Salmon Data:

* Washington State Department of Fish and Wildlife
	* [Strait of Juan de Fuca Summer Chum](https://fortress.wa.gov/dfw/score/score/species/population_details.jsp?stockId=2500)
	* [Hood Canal Summer Chum](https://fortress.wa.gov/dfw/score/score/species/population_details.jsp?stockId=2300)

## Project tools
This project was completed using open-source software tools, underpinned by the statistical programming language R. Follow the links below to learn more about the tools used, including installation instructions.

* [The R Project for Statistical Computing](https://www.r-project.org/): R is a free software environment for statistical computing and graphics. 
* [R Studio](https://www.rstudio.com/): Open source and enterprise-ready professional integrated development environment \(IDE\) for R.
* [Leaflet for R](https://rstudio.github.io/leaflet/): Leaflet is one of the most popular open-source JavaScript libraries for interactive maps. This R package makes it easy to integrate and control Leaflet maps in R.
* [Shiny](https://shiny.rstudio.com/): A web application framework for R.
* Additional R packages used: 
	* [tidyverse](http://tidyverse.org/): An ecosystem of packages designed with common APIs and a shared philosophy.
	* [forcats](http://forcats.tidyverse.org/): A suite of useful tools that solve common problems with factors.
	* [ggplot2](http://ggplot2.tidyverse.org/): A system for declaratively creating graphics.
	* [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html): Provides bindings for the Geospatial Data Abstraction Library.
	* [MazamaSpatialUtils](https://cran.r-project.org/web/packages/MazamaSpatialUtils/index.html): A suite of conversion scripts to create internally standardized spatial polygons dataframes.
	* [rmapshaper](https://cran.r-project.org/web/packages/rmapshaper/index.html): Edit and simplify 'geojson' and 'Spatial' objects.
	* [stringr](https://cran.r-project.org/web/packages/stringr/index.html): Simple, consistent wrappers for common string operations

## Usage
Scripts created for data cleaning and analysis pipeline:

* Project investment data: \(final link to come\)
* Chum salmon data: \(final link to come\)
* Water quality data: \(final link to come\)

Script created for Shiny web-based visualization prototype:

* Shiny web app: https://github.com/katger4/psp-tek/blob/master/shinyapp/app.R

## Key terminology

* [TSS, or Total Suspended Solids](https://www.ndhealth.gov/WQ/SW/Z6_WQ_Standards/WQ_TSS.htm): Solid materials, including organic and inorganic, that are suspended in the water
* [Turbidity](https://water.usgs.gov/edu/turbidity.html): The measure of relative clarity of a liquid.
* [HUC, or Hydrologic Unit Code](https://water.usgs.gov/GIS/huc.html): A unique code identifying a hydrologic unit such as a region, sub-region, watershed, or catchment. Smaller HUC units are nested inside larger HUC units. Analysis for this project, for instance, is at the HUC-10 and HUC-12 levels, with HUC-12 units nested inside HUC-10 units.

## xxxxx
xxxxx

## Acknowledgments
We are grateful for the support we've received from our partners:

* Nic Weber, University of Washington's [Open Data Literacy](https://odl.ischool.uw.edu/)
* Leska Fore, [Puget Sound Partnership](http://www.psp.wa.gov/)
* Keith Dublanica, [Governor's Salmon Recovery Office](http://www.rco.wa.gov/salmon_recovery/gsro.shtml)
* Chantell Krider, [South Sound Spatial](https://www.southsoundspatial.com/)