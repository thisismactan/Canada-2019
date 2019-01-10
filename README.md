# Canada-2019
## Last updated: 9 Jan 2019
This is the repository for the Election StatSheet 2019 Canadian election forecast. Hooray!

Current status: testing out some models, shaping campaign contributions data.

Does anyone know how to use spTransform() to change a shapefile to Lambert Conformal Conic projection?

### Notes to self ###
- Final model must predict all parties' share of the vote simultaneously allowing for correlations between parties
- Currently waiting on: **StatCan to get back to me on electoral district demographics 2001 - 2011**

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
  - **/Miscellanea**

### What's in the box?
- **Code**: All scripts, including data processing, model building, simulation, etc.
- **Data**: Raw data downloaded from websites (see repo wiki). After processing, the processed versions are saved to the Processed directory. Also, shapefiles for creating maps.
- **Miscellanea**: For now, these are just images I want to add to the wiki.

