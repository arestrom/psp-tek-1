# Effectiveness and Evaluation Tool
Tim Blankemeyer, Emma Clarke, and Katrina Gertz are a group of MLIS Candidates from the University of Washington's Information School who are interested in leveraging open data and open-source tools to help solve complex problems.

Numerous environmental restoration projects have been undertaken throughout the Puget Sound, but **connecting investments in these projects to co-located indicators of habitat viability** is challenging. For this [Open Data Literacy (ODL) Capstone project](https://ischool.uw.edu/capstone/projects/2017/opening-data-visualizing-effectiveness-puget-sound-restoration-efforts), we've collaborated with the Puget Sound Partnership, the Governor's Salmon Recovery Office, South Sound Spatial, and other partners to design a scalable data cleaning and analysis pipeline as well as an interactive visualization prototype to show what's working to restore Puget Sound. 

**CAVEAT:** This is a prototype to show what a web-based analysis tool *might* look like. The underlying data for water quality and salmon were sourced from public web sites. Data and results have not been vetted or approved; that is the next step.

See the prototype at https://ejclarke.shinyapps.io/capstone/.

## Contact
Any questions or comments can be directed to Leska Fore from the Puget Sound Partnership at leska.fore@psp.wa.gov.

## Table of Contents

- [Data Sources](#data-sources)
    + [Project investment Data](#project-investment-data)
    + [Water Quality Data](#water-quality-data)
    + [Hood Canal Summer Chum Salmon Data](#hood-canal-summer-chum-salmon-data)
    + [Hydrologic Unit Shapefile Data](#hydrologic-unit-shapefile-data)
- [Project tools](#project-tools)
- [Usage](#usage)
- [Key terminology](#key-terminology)
- [xxxxx](#xxxxx)
- [Acknowledgments](#acknowledgments)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>

## Data Sources
As mentioned in the Description, the underlying data for the prototype project were sourced from public web sites.

#### Project investment Data

* [Ecology's Administration of Grants and Loans (EAGL) Investment Projects](http://www.ecy.wa.gov/funding/EAGL.html):
	* Note: Access to data requires log-in
* [PRoject Information SysteM (PRISM) Investment Projects](http://www.rco.wa.gov/prism_app/about_prism.shtml)
	* Note: Access to data requires log-in

#### Water Quality Data

* [Washington State Department of Ecology's Environmental Information Management (EIM)](http://www.ecy.wa.gov/eim/)
	* Search for data limited to result parameters Turbidity and TSS, and WRIA's Kennedy-Goldsborough (14), Kitsap (15), Skokomish-Dosewallips (16), Quilcence-Snow (17), and Elwah-Dungeness (18).

#### Hood Canal Summer Chum Salmon Data

* Washington State Department of Fish and Wildlife
	* [Strait of Juan de Fuca Summer Chum](https://fortress.wa.gov/dfw/score/score/species/population_details.jsp?stockId=2500)
	* [Hood Canal Summer Chum](https://fortress.wa.gov/dfw/score/score/species/population_details.jsp?stockId=2300)

#### Hydrologic Unit Shapefile Data

* [U.S. Geological Survey - Watershed Boundary Dataset (WBD)](https://nhd.usgs.gov/wbd.html)
	* Note: Pacific Northwest region shapefiles are **WBD_17_Shape.zip**. [See XML metadata](ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Hydrography/WBD/HU2/Shape/WBD_17_Shape.xml).

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
	* [spdplyr](https://cran.r-project.org/web/packages/spdplyr/index.html): Data manipulation verbs for the spatial data classes.
	* [MazamaSpatialUtils](https://cran.r-project.org/web/packages/MazamaSpatialUtils/index.html): A suite of conversion scripts to create internally standardized spatial polygons dataframes.
	* [rmapshaper](https://cran.r-project.org/web/packages/rmapshaper/index.html): Edit and simplify 'geojson' and 'Spatial' objects.
	* [stringr](https://cran.r-project.org/web/packages/stringr/index.html): Simple, consistent wrappers for common string operations

## Tool usage and data pipeline



Scripts created for data cleaning and analysis pipeline:

* Project investment data: \(final link to come\)
* Chum salmon data: \(final link to come\)
* Water quality data: \(final link to come\)

Script created for Shiny web-based visualization prototype:

* Shiny web app: https://github.com/katger4/psp-tek/blob/master/shinyapp/app.R

## Key terminology

* [Chum salmon](https://en.wikipedia.org/wiki/Chum_salmon): _Oncorhynchus keta_, a species of anadromous fish in the salmon family.
* [TSS, or Total Suspended Solids](https://www.ndhealth.gov/WQ/SW/Z6_WQ_Standards/WQ_TSS.htm): Solid materials, including organic and inorganic, that are suspended in the water
* [Turbidity](https://water.usgs.gov/edu/turbidity.html): The measure of relative clarity of a liquid.
* [HUC, or Hydrologic Unit Code](https://water.usgs.gov/GIS/huc.html): A unique code identifying a hydrologic unit such as a region, sub-region, watershed, or catchment. Smaller HUC units are nested inside larger HUC units. Analysis for this project, for instance, is at the HUC-10 and HUC-12 levels, with HUC-12 units nested inside HUC-10 units.

## Data download variable definitions
The project's web-based visualization prototype includes functionality to download CSV files for the investment and outcome data sets used to create the visualizations. Here are definitions for all four of the downloadable data sets:

#### Chum Salmon

* __year:__ Measurement year
* __site:__ Measurement site and type, corresponding to label used by WA Dept. of Fish & Wildlife
* __name:__ Measurement site, isolated
* __project_cat:__ Measurement type, isolated
* __lon:__ Measurement site longitude
* __lat:__ Measurement site latitude
* __description:__ Measurement site description, taken from WA Dept. of Fish & Wildlife web site
* __HUC_id:__ Hydrologic Unit Code for measurement site
* __HUC_Name:__ Hydrologic Unit Name for measurement site
* __medianyr:__ Median investment project year for HUC containing measurement site
* __cohensd:__ Cohen's D value for measurement site
* __site_mean_after:__ Mean of measurements after medianyr
* __site_mean_before:__ Mean of measurements before medianyr
* __site_sd_before:__ Standard deviation \(SD\) of measurements before medianyr
* __site_sd_after:__ Standard deviation \(SD\) of measurements after medianyr
* __var_pooled:__ xxxx
* __var_cohensd:__ xxxx
* __sd_cohensd:__ xxxx
* __wsubi:__ xxxx
* __wsubixd:__ xxxx
* __cohensd_huc_mean:__ Mean Cohen's D value of measurements aggregated by HUC
* __huc_mean_after:__ Mean of measurements after medianyr, aggregated by HUC
* __huc_mean_before:__ Mean of measurements before medianyr, aggregated by HUC
* __sum_wsubixd:__ xxxx
* __sum_wsubi:__ xxxx
* __cohensd_huc_var:__ Variance of Cohen's D value of measurements aggregated by HUC
* __cohensd_huc_sd:__ Standard deviation (SD) of Cohen's D value of measurements aggregated by HUC
* __plus_minus:__ xxxx
* __TimePeriod:__ Categorical value placing measurement before, during, or after medianyr
* __measurement:__ Measurement at given site in given year
* __effectsize:__ Categorical value of mean Cohen's D value in given HUC
* __site_effectsize:__ Categorical value of Cohen's D value at given measurement site
* __status:__ Categorical value noting change direction of mean Cohen's D value in given HUC
* __coloreffect:__ HEX code color assignment for given effectsize and status
* __colorblind:__ HEX code color assignment (colorblind-friendly version) for given effectsize and status
* __result_type:__ Categorical value corresponding to variable
* __unit:__ Unit of measurement, if any
* __HUC_level:__ Hydrologic Unit Code (HUC) level, either 10 or 12, for given row of data
* __project_source:__ Categorical variable corresponding to project source (PRISM or EAGL) - _NA for chum salmon data_

#### Turbidity

* xxxx

#### TSS

* xxxx

#### Investment

* __year:__ Project investment year
* __name:__ Project investment name
* __project_cat:__ Project investment category
* __lon:__ Project investment site longitude
* __lat:__ Project investment site latitude
* __HUC_id:__ Hydrologic Unit Code for project investment site
* __HUC_Name:__ Hydrologic Unit Name for project investment site
* __measurement:__ Amount of given project investment
* __result_type:__ Categorical value corresponding to variable
* __unit:__ Unit of measurement, if any
* __HUC_level:__ Hydrologic Unit Code (HUC) level, either 10 or 12, for given row of data
* __Study_ID:__ Unique ID for project investment
* __project_source:__ Categorical variable corresponding to project source (PRISM or EAGL)
* __color:__ HEX code color assignment corresponding to categorical investment value (small, medium, or large)

## Acknowledgments
We are grateful for the support we've received from our partners:

* Nic Weber, University of Washington's [Open Data Literacy](https://odl.ischool.uw.edu/)
* Leska Fore, [Puget Sound Partnership](http://www.psp.wa.gov/)
* Keith Dublanica, [Governor's Salmon Recovery Office](http://www.rco.wa.gov/salmon_recovery/gsro.shtml)
* Chantell Krider, [South Sound Spatial](https://www.southsoundspatial.com/)