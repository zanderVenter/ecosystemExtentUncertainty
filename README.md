# ecosystemExtentUncertainty
A repository for the data and code in support of research paper on ecosystem extent accounting from satellite-based maps: https://www.sciencedirect.com/science/article/pii/S2212041624000056

The workflow covers two platforms: Google Earth Engine (GEE) and R. GEE is used to (1) pre-process Dynamic World and the custom ELC10 to produce change maps for the three epochs, (2) conduct pixel counting for each product and epoch combination, (3) to generate stratified random samples for verification/labelling by image interpreters, and (4) to host the web app which allows samplers/interpreters to label the samples using VHR imagery. R is used to perform the design-based area estimation, produce graphs and tables.

It is important to note that step 1 above uses ELC10 code from [Venter & Sydenham 2021](https://www.mdpi.com/2072-4292/13/12/2301). Step 3 uses code from [AREA2 GEE tool](https://area2.readthedocs.io/en/latest/sampling_str.html). Step 4 uses code from the [mapaccuracy](https://cran.r-project.org/web/packages/mapaccuracy/mapaccuracy.pdf) package in R. Ideally, all these steps could be coded in one platform/package. However, we have not had the resources to do that.

The scripts are run in this order:
1. 'samples_generate.js' - this pre-processes the DW and ELC10 data and exports samples to be labelled for design-based area estimation.
2. 'sampling_app.js' -  this codes the user interface which allows image interpreters to collect the reference data (ie. label the samples produced in the first script)
3. 'setup.R' - this installs R libraries and sets some global functions for the R environment.
4. 'sampler_correspondence.R' - this assesses the correspondence between samplers based on a calibration set where each sample was labelled by multiple samplers
5. 'area_estimation.R' - this is where the design-based area estimation takes place and where I make all the figures and tables in the paper.

