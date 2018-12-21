# Canada-2019
## Last updated: 21 Dec 2018
This is the repository for the Election StatSheet 2019 Canadian election forecast. Hooray!

Current status: data processing; figuring out how I actually plan to model this. Does anyone know how to use spTransform() to change a shapefile to Lambert Conformal Conic projection?

## Directory structure
- **/Canada-2019**
  - **/Code**
    - **/Data processing**
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

## Sources for raw data
- **Election results data** are downloadable from Elections Canada: http://www.elections.ca/content.aspx?section=ele&lang=e
- **Candidate lists** are also downloadable from Elections Canada, albeit with some mild manual pre-cleaning by me
- **Shapefiles** are also also downloadable from Elections Canada: https://open.canada.ca/data/en/dataset/737be5ea-27cf-48a3-91d6-e835f11834b0 
- **Demographics data** are downloadable from Statistics Canada highlight tables, with some moderate manual pre-cleaning: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/index-eng.cfm
- **Polling data** is scraped from polls listed on the Wikipedia articles for the polling in general elections, e.g. https://en.wikipedia.org/wiki/Opinion_polling_in_the_43rd_Canadian_federal_election. National data is directly scraped; provincial data is compiled by hand.