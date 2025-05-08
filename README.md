This repository contains the code and graphics used in my master's thesis: 

# A Synergistic Use of Optical and SAR Remote Sensing Data for the Long-term Evolution of Biocrusts and their Activity Across Diverse Dryland Ecosystems

You can have a look at the thesis here: [MSc_Thesis_Biocrusts.pdf](https://github.com/user-attachments/files/20109738/MSc_Thesis_Rieser_Biocrusts.pdf)

**Table of contents:**

**1) Graphics:**\
   All graphics and maps included in the thesis and the appendix
   
**2) Results:**\
   Sorted by study area are
   - Resulting classification and probability maps as GeoTIFF files
   - Accuracy assessment, variable importance metrics and spatial share of the classes as Excel spreadsheets

**3) Scripts:**

- Google Earth Engine Scripts tp process satellite time series data
  - Sentinel-2 and Landsat 4-8 processing chain includes includes extensive cloud, shadow and snow masking, calculation of multispectral indices, harmonization between Sentinel 2 and Landsat 4-8, and finally the spatiotemporal metrics per study area
  - Sentinel-1 processing chain includes terrain flattening, speckle filtering, mathematical band combination and the calculation of spatiotemporal metrics for each study area
- R-Scripts for postprocessing and statistical analysis
  - Random forest classification and regression
  - Calculating time series statistics
  - Visualizing the time series and classification results
- R-Scripts for downloading meteorological data from DWD and AEMET

