Terrace Extraction
==================
In this subfolder are the necessary files to use the script tool on WinGRASS versions. (Tested on 7.x.)
- To the best of my knowledge it is up to date and works, but please compare the change date to the version in root.


This README provides the information to install r.terrace.geom.


Dependencies:
-------------

-   recommended GRASS GIS 7.x
-   Python packages (os, platform, sys, subprocess, csv, grass.script, grass.exceptions)
-   GRASS GIS addon r.geomorphon 
    https://grass.osgeo.org/grass72/manuals/addons/r.geomorphon.html

Installation:
-------------
* Supposing you have a GRASS GIS version installed.

1.  Install GRASS GIS addon
    (g.extension extension=r.geomorphon operation=add)
       * otherwise the tool will inform you, that you miss it

2.  Copy r.terrace.geom.py script to path\to\grassversion\scripts folder (e.g. X:\Program Files\GRASS GIS 7.2.0\scripts)
       * this is the Python script loading the R script to do the calculations and create the plots & also to run r.geomorphon if set
3.  Copy TERRACE_jozsa.R script to path\to\grassversion\scripts folder
       * otherwise the tool will inform you to put it there
       * tool will automatically install necessary packages, so first time it requires permission to install (or install manually)
4.  Copy r.terrace.geom.bat to path\to\grassversion\bin folder
5.  Copy r.terrace.geom.html help page to path\to\grassversion\docs\html folder

6.  Open GRASS GIS and run command r.terrace.geom - the tool should work and you should see the available information on manual page
