# Canada-2019
## Last updated: 28 Jan 2019
This is the repository for the Election StatSheet 2019 Canadian election forecast. Hooray!

Current status: testing random forest models, estimating model error variances, figuring out how to scrape tables from a drop-down menu at this URL: https://www12.statcan.gc.ca/english/census01/products/standard/fedprofile/SelectFED.cfm?R=FED03 

Does anyone know how to use spTransform() to change a shapefile to Lambert Conformal Conic projection?

### Notes to self ###
- Final model must predict all parties' share of the vote simultaneously allowing for correlations between parties

## Directory structure
- **/Canada-2019**
  - **/Code**
    - **/Data processing**
    - **/Modeling**
  - **/Data**
    - **/Processed**
    - **/Raw**
      - **/2004 results**
      - **/2006 results**
      - **/2008 results**
      - **/2011 results**
      - **/2015 results**
      - **/Demographics**
    - **/Shapefiles**
  - **/Output**
    - **/Model graphs**
    - **/Model testing**

### What's in the box?
- **Code**: All scripts, including data processing, model building, simulation, etc.
- **Data**: Raw data downloaded from websites (see repo wiki). After processing, the processed versions are saved to the Processed directory. Also, shapefiles for creating maps.
- **Output**: Things generally for publication.

