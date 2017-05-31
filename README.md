# Effectiveness and Evaluation Tool
## Description
Tim Blankemeyer, Emma Clarke, and Katrina Gertz are a group of MLIS Candidates from the University of Washington's Information School who are interested in leveraging open data and open-source tools to help solve complex problems.

Numerous environmental restoration projects have been undertaken throughout the Puget Sound, but **connecting investments in these projects to co-located indicators of habitat viability** is challenging. For this Open Data Literacy (ODL) Capstone project, we've collaborated with the Puget Sound Partnership, the Governor's Salmon Recovery Office, and South Sound Spatial to design a scalable data cleaning and analysis pipeline as well as an interactive visualization prototype to show what's working to restore Puget Sound. 

**CAVEAT:** This is a prototype to show what a web-based analysis tool *might* look like. The underlying data for water quality and salmon were sourced from public web sites. Data and results have not been vetted or approved; that is our next step.

See the prototype at https://ejclarke.shinyapps.io/capstone/.

## Contact
Any questions or comments can be directed to Leska Fore from the Puget Sound Partnership at leska.fore@psp.wa.gov.

## Data Sources
As mentioned in the Description, the underlying data for the prototype project were sourced from public web sites.

#### Project investment Data:

*[Ecology's Administration of Grants and Loans (EAGL) Investment Projects](http://www.ecy.wa.gov/funding/EAGL.html):
	*Note: Access to data requires log-in
*[PRoject Information SysteM (PRISM) Investment Projects](http://www.rco.wa.gov/prism_app/about_prism.shtml)
	*Note: Access to data requires log-in

#### Water Quality Data:

* [Washington State Department of Ecology's Environmental Information Management (EIM)](http://www.ecy.wa.gov/eim/)
	* Search for data limited to result parameters Turbidity and TSS, and WRIA's Kennedy-Goldsborough (14), Kitsap (15), Skokomish-Dosewallips (16), Quilcence-Snow (17), and Elwah-Dungeness (18).

#### Hood Canal Summer Chum Salmon Data:

* Washington State Department of Fish and Wildlife
	* [Strait of Juan de Fuca Summer Chum](https://fortress.wa.gov/dfw/score/score/species/population_details.jsp?stockId=2500)
	* [Hood Canal Summer Chum](https://fortress.wa.gov/dfw/score/score/species/population_details.jsp?stockId=2300)

## Project tools
This project was built using open-source software tools -- R \(including several packages\), Leaflet, and Shiny. Follow the links below to learn more about the software used, including installation instructions.

* [The R Project for Statistical Computing](https://www.r-project.org/): R is a free software environment for statistical computing and graphics. 
* [R Studio](https://www.rstudio.com/): Open source and enterprise-ready professional integrated development environment \(IDE\) for R.
* [Leaflet for R](https://rstudio.github.io/leaflet/): Leaflet is one of the most popular open-source JavaScript libraries for interactive maps. This R package makes it easy to integrate and control Leaflet maps in R.
* [Shiny](https://shiny.rstudio.com/): A web application framework for R.
* Additional R packages used: 
	* [tidyverse](http://tidyverse.org/): an ecosystem of packages designed with common APIs and a shared philosophy.
	* [forcats](http://forcats.tidyverse.org/): a suite of useful tools that solve common problems with factors.
	* [ggplot2](http://ggplot2.tidyverse.org/): a system for declaratively creating graphics.
	* [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html): provides bindings for the Geospatial Data Abstraction Library.
	* [MazamaSpatialUtils](https://cran.r-project.org/web/packages/MazamaSpatialUtils/index.html): a suite of conversion scripts to create internally standardized spatial polygons dataframes.
	* [rmapshaper](https://cran.r-project.org/web/packages/rmapshaper/index.html): edit and simplify 'geojson' and 'Spatial' objects.
	* [stringr](https://cran.r-project.org/web/packages/stringr/index.html): simple, consistent wrappers for common string operations


## Usage
Scripts created for data cleaning and analysis pipeline:

* Project investment data: \(final link to come\)
* Chum salmon data: \(final link to come\)
* Water quality data: \(final link to come\)

Script created for Shiny web-based visualization prototype:

* Shiny web app: https://github.com/katger4/psp-tek/blob/master/shinyapp/app.R

## xxxxxx
xxxxx

## xxxxx
xxxxx

## Acknowledgments
We are grateful for the support we've received from our partners:

* Nic Weber, University of Washington's [Open Data Literacy](https://odl.ischool.uw.edu/)
* Leska Fore, [Puget Sound Partnership](http://www.psp.wa.gov/)
* Keith Dublanica, [Governor's Salmon Recovery Office](http://www.rco.wa.gov/salmon_recovery/gsro.shtml)
* Chantell Krider, [South Sound Spatial](https://www.southsoundspatial.com/)


    [bug]     /"*._         _
          .-*'`    `*-.._.-'/
        < * ))     ,       ( 
          `*-._`._(__.--*"`.\