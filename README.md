# Canada-2019
## Last updated: 5 Apr 2019
This is the repository for the Election StatSheet 2019 Canadian election forecast. Hooray!

Current status: estimating model error variances, compiling federal electoral district demographic data.

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

