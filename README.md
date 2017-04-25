# Terrace Extraction

In this subfolder are the necessary files to use the script tool on GRASS GIS versions on Linux (Tested on Ubuntu with 7.x.)
- To the best of my knowledge it is up to date and works, but please compare the change date to the version in root.


This README provides the information to install r.terrace.geom.


## Dependencies:

-   GRASS GIS 7.2
-   R 3.x (packages: spgrass6/rgrass7, ggplot2, plyr)
-   Python packages (os, platform, sys, subprocess, csv, grass.script, grass.exceptions)
-   GRASS GIS addon r.geomorphon 
    https://grass.osgeo.org/grass72/manuals/addons/r.geomorphon.html

## Installation:
* Supposing you have a GRASS GIS 7.2 installed.

1.  Install GRASS GIS addon
    (g.extension extension=r.geomorphon operation=add)
       * otherwise the tool will inform you, that you miss it
    
2.  Install r.terrace.geom easy way: 
    g.extension extension=r.terrace.geom operation=add url=https://github.com/edinaj0zs4/terrace_extraction_grassgis
       * for other installation solutions see the subfolders or follow description <a href="https://grasswiki.osgeo.org/wiki/Compile_and_Install#Scripts">here</a>.
3.  Copy TERRACE_jozsa.R script to path/to/grassaddons/scripts folder ($HOME/.grass7/addons/scripts)
       * otherwise the tool will inform you to put it there
       * tool will automatically install necessary packages

4.  Open GRASS GIS and run command r.terrace.geom - the tool should work and you should see the available information on manual page

#### Notes:
**under development**<br>
Aim of the project is to create a raster add-on for GRASS GIS, that extracts terrace-like surfaces from digital elevation models and also creates a pdf plot with graphs showing the terrain characteristics and logic of process.
This is part of my PhD research regarding DEM/DSM based geomorphological mapping with semi-automated landform delineation algorithms.
The tool works, but the codes and possibly the algorithm could be further improved.

#### Output examples based on artificial terrain:
![thresholds](https://cloud.githubusercontent.com/assets/25442728/25378225/16ff81a6-29aa-11e7-9b05-32c30b0096c0.png)
![swathprofile](https://cloud.githubusercontent.com/assets/25442728/25378231/1bed7aba-29aa-11e7-819d-a7709f4b1ca3.png)
![scatter](https://cloud.githubusercontent.com/assets/25442728/25378240/20dc0dac-29aa-11e7-904f-f7bad250d481.png)
![selection](https://cloud.githubusercontent.com/assets/25442728/25378242/23c6de98-29aa-11e7-9841-75e4e457693d.png)
![frequency](https://cloud.githubusercontent.com/assets/25442728/25378247/26a8c7f2-29aa-11e7-95aa-a58bac1c18da.png)
![longprof](https://cloud.githubusercontent.com/assets/25442728/25378249/2979f136-29aa-11e7-99bd-749c07640b67.png)

#### Acknowledgements:
The author would like to express her gratitude for the colleagues of the Department of Physical and Environmental Geography for the professional advices on the project and the support of the Doctoral School of Earth Sciences, University of Pécs. The present scientific contribution is dedicated to the 650th anniversary of the foundation of the University of Pécs.
_The research of Edina Józsa was supported by the Human Capacities Grant Management Office and the Hungarian Ministry of Human Capacities in the framework of the NTP-NFTÖ-16 project._
