# Canada-2019
## Last updated: 11 July 2019
This is the repository for the Election StatSheet 2019 Canadian election forecast. Hooray!

Current status: fully incorporating district-level polling into all parts of the forecast.

### Notes to self ###

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
  - **/Shiny-app**

### What's in the box?
- **Code**: All scripts, including data processing, model building, simulation, etc.
- **Data**: Raw data downloaded from websites (see repo wiki). After processing, the processed versions are saved to the Processed directory. Also, shapefiles for creating maps.
- **Output**: Things generally for publication.
- **Shiny-app**: Everything to be deployed with the Shiny app.

## Acknowledgments
Thanks are in order to:
- Imane Baitsa and Jérémie Darrieu at Statistics Canada for pointing me to some archived data sources on the StatCan website;
- [Kaushik Mohan](https://github.com/kaushik12) for his R Shiny assistance; and
- [Gabriel Guzman](https://github.com/gabriel1200) for light QA on the Shiny app.