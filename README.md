# Canada-2019
## Last updated: 20 Nov 2018
This is the repository for the Election StatSheet 2019 Canadian election forecast. Hooray!

Current status: data processing; figuring out how I actually plan to model this.

## Directory structure
- **/Canada-2019**
  - **/Data**
    - **/Demographics**
    - **/Processed**
    - **/Raw**
      - **/2015 results**
  - **/Data processing code**
  
## Sources for raw data
- **Election results data** are downloadable from Elections Canada: http://www.elections.ca/content.aspx?section=ele&lang=e
- **Candidate lists** are also downloadable from Elections Canada, albeit with some mild manual pre-cleaning by me
- **Demographics data** are downloadable from Statistics Canada highlight tables, with some moderate manual pre-cleaning: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/index-eng.cfm
- **Polling data** is compiled by hand from polls listed on the Wikipedia articles for the polling in general elections, e.g. https://en.wikipedia.org/wiki/Opinion_polling_in_the_43rd_Canadian_federal_election 