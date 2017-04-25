#!/usr/bin/env python
#
"""
MODULE:    r_terrace_geom.py

AUTHOR(S): Edina Jozsa <edina.j0zs4 AT gmail.com>


PURPOSE:   This flexible terrain analysing tool is developed for the
           delineation and quantifiable analysis of terrace remnants.
           The algorithm determines cells potentially belonging to
           terrace surfaces based on local slope characteristics
           and a minimum area size threshold.

NOTES:     The algorithm cuts up the area into parallel sections in
           flow direction and determines cells belonging to terrace-like
           surfaces. As a result an output report is created that contains
           a histogram of altitudes, a swath profile of the landscape,
           scatter plots to represent the relation of the relative elevations
           and slope values in the analysed sections and two final plots
           showing the longitudinal profile of the river with the determined
           height ranges of terrace levels and a histogram of the values.

DEPENDENCIES:    R 3.x (packages: spgrass6/rgrass7, ggplot2, plyr) &
            r.geomorphon add-on

COPYRIGHT: (C) 2015-2017 Edina Jozsa
           and the GRASS Development Team

           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.


REFERENCES:
Demoulin, A., Bovy, B., Rixhon, G., Cornet, Y., 2007. An automated
method to extract fluvial terraces from digital elevation models.
The Vesdre valley, a case study in Eastern Belgium.
In: Geomorphology 91 (1-2), pp. 51-64.

"""

#%Module
#% description: Extract terraces from DEM
#% keyword: raster
#% keyword: terrain
#% keyword: geomorphology
#% keyword: terraces
#% keyword: landform
#%End

#%option G_OPT_R_ELEV
#% key: elevation
#% description: Name of input elevation model
#% guisection: Elevation
#%end

#%flag
#% key: t
#% description: Remove valleys of tributaries based on geomorphons map
#% guisection: Elevation
#%end

#%option G_OPT_R_INPUT
#% key: slope_percent
#% description: Name of input slope percent map
#% required : no
#% guisection: Elevation
#%end

#%flag
#% key: f
#% description: Filter slope map
#% guisection: Elevation
#%end

#%option
#% key: limit
#% type: integer
#% answer: 999
#% description: Altitude limit for terrace extraction (meter a.s.l.)
#% required : no
#% guisection: Elevation
#%end

#%option G_OPT_R_INPUT
#% key: raster_water
#% description: Name of input raster watercourse map (prepare with v.to.rast)
#% required : yes
#% guisection: Watercourse
#%end

#%flag
#% key: r
#% description: Analyse right bank area (set only if elevation is total watershed)
#% guisection: Watercourse
#%end

#%flag
#% key: l
#% description: Analyse left bank area (set only if elevation is total watershed)
#% guisection: Watercourse
#%end

#%option
#% key: flowdir
#% type: string
#% description: General flow direction of the watercourse
#% required : yes
#% options: NS, SN, EW, WE
#% multiple: no
#% guisection: Watercourse
#%end

#%option
#% key: azimuth
#% type: integer
#% answer: 999
#% description: Azimuth to north in degrees
#% required : no
#% guisection: Watercourse
#%end

#%option G_OPT_F_OUTPUT
#% key: report
#% description: Name for output report (path\to\name.pdf)
#% required: yes
#% guisection: Output
#%end

#%option G_OPT_R_OUTPUT
#% key: terrace_map
#% description: Name for output terrace map
#% required: yes
#% guisection: Output
#%end

#%option
#% key: terrlevel
#% type: string
#% answer: 999,999
#% multiple: yes
#% description: Altitude ranges of terrace levels seperated by comma (e.g. 100,105,107,111)
#% guisection: Output
#%end

import os
import platform
import sys
import subprocess
import csv
import grass.script as grass
from grass.exceptions import CalledModuleError

def main():
    if platform.system() == 'Windows':
        try:
            import winreg
        except ImportError:
            import _winreg as winreg

        try:
            try:
                key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, 'SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall', 0, winreg.KEY_READ | winreg.KEY_WOW64_64KEY)
                count = (winreg.QueryInfoKey(key)[0])-1
                while (count >= 0):
                    subkeyR = winreg.EnumKey(key, count)
                    if subkeyR.startswith('R for'):
                        count = -1
                    else:
                        count = count-1
                winreg.CloseKey(key)
                key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, str('SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\' + subkeyR), 0, winreg.KEY_READ | winreg.KEY_WOW64_64KEY)
                value = winreg.QueryValueEx(key, 'InstallLocation')[0]
                winreg.CloseKey(key)
            except:
                key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, 'SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall', 0, winreg.KEY_READ | winreg.KEY_WOW64_32KEY)
                count = (winreg.QueryInfoKey(key)[0])-1
                while (count >= 0):
                    subkeyR = winreg.EnumKey(key, count)
                    if subkeyR.startswith('R for'):
                        count = -1
                    else:
                        count = count-1
                winreg.CloseKey(key)
                key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, str('SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\' + subkeyR), 0, winreg.KEY_READ | winreg.KEY_WOW64_64KEY)
                value = winreg.QueryValueEx(key, 'InstallLocation')[0]
                winreg.CloseKey(key)
            grass.message(_("R is installed!"))
            pathtor = os.path.join(value, 'bin\\Rscript')
        except:
            grass.fatal("Please install R!")

    else:
        try:
            subprocess.call(['which', 'R'])
            grass.message(_("R is installed!"))
            pathtor = 'Rscript'
        except:
            grass.fatal("Please install R!")
### Prepare elevation map
    elevation = str(options['elevation']).split('@')[0]
    incheck = grass.find_file(name=elevation, element='cell')
    if not incheck['file']:
        grass.fatal("Raster map <%s> not found" % elevation)
    grass.use_temp_region()
    grass.run_command('g.region', rast=elevation)
    gregion = grass.region()
    res = int(round(gregion['nsres']))

    ### Prepare slope map
    slope = str(options['slope_percent']).split('@')[0]
    if slope:
        incheck = grass.find_file(name=slope, element='cell')
        if not incheck['file']:
            grass.fatal("Raster map <%s> not found" % slope)
    else:
        grass.message(_("Generating slope percent map"))
        slope = str('temp_' + elevation + '_slope')
        grass.run_command('r.slope.aspect', elevation=elevation, slope=slope, format='percent')

    ## Filter slope map
    filter = flags['f']
    if filter:
        grass.message(_("Low-pass filter on slope percent map"))
        slope_filter = str(slope + '_filter')
        filter_size = round(150/res)
        if filter_size/2 == 0:
            filter_size = filter_size + 1
        grass.run_command('r.neighbors', input=slope, output=slope_filter, method='minimum', size=int(filter_size), flags='c')
        slope = slope_filter

    remvalley = flags['t']
    if remvalley:
    ## Check if r.geomorphon is installed
        if not grass.find_program('r.geomorphon', '--help'):
            grass.fatal("r.geomorphon is not installed")
        else:
            # Create temporary geomorphons map, to be able to map wider forms 300 meter is set as search radius (fits previous findings and SRTM)
            grass.message(_("Generating geomorphons map to remove valleys of tributaries"))
            geom_map = str('temp_'+ elevation + '_geomorphons')
            try:
                grass.run_command('r.geomorphon', elevation=elevation, search=330, skip=1, flat=0.7, dist=0, forms=geom_map, flags='m')
            except:
                grass.run_command('r.geomorphon', dem=elevation, search=330, skip=1, flat=0.7, dist=0, forms=geom_map, flags='m')
            # Remove cells of valleys and footslopes from elevation map [remove: hollow, valley, depression // keep: flat, summit, ridge, shoulder, spur, slope, footslope]
            notrib = str('temp_' + elevation + '_remvalleys')
            grass.mapcalc('$outmap = if($geom_map == 7 ||| $geom_map >= 9, null(), $elevation)', outmap=notrib, geom_map=geom_map, elevation=elevation)
            elevation = notrib

    limit = int(options['limit'])

### Prepare watercourse
    watercourse = str(options['raster_water']).split('@')[0]
    azimuth = int(options['azimuth'])
    ## Select which bank to analyse
    left = flags['l']
    right = flags['r']
    dir = str(options['flowdir'])
    if left or right:
        grass.run_command('g.region', rast=watercourse)
        waterarea = grass.region(watercourse)
        grass.run_command('g.region', rast=elevation)
        elevarea = grass.region(elevation)
        if left: ## user set left side to analyse
            if dir == 'NS': ## side depends on flow direction
                Nw = int(waterarea.n)
                Sw = int(waterarea.s)
                Ee = int(elevarea.e)
                Ww = int(waterarea.w)
                grass.run_command('g.region', n=Nw, s=Sw, e=Ee, w=Ww, flags='a')
            elif dir == 'SN':
                Nw = int(waterarea.n)
                Sw = int(waterarea.s)
                Ew = int(waterarea.e)
                We = int(elevarea.w)
                grass.run_command('g.region', n=Nw, s=Sw, e=Ew, w=We, flags='a')
            elif dir == 'WE':
                Ne = int(elevarea.n)
                Sw = int(waterarea.s)
                Ew = int(waterarea.e)
                Ww = int(waterarea.w)
                grass.run_command('g.region', n=Ne, s=Sw, e=Ew, w=Ww, flags='a')
            else:
                Nw = int(waterarea.n)
                Se = int(elevarea.s)
                Ew = int(waterarea.e)
                Ww = int(waterarea.w)
                grass.run_command('g.region', n=Nw, s=Se, e=Ew, w=Ww, flags='a')
            area = str('temp_' + elevation + '_div')
            grass.mapcalc('$outmap = if($watercourse > 0, null(), $elevation / $elevation)', outmap=area, watercourse=watercourse, elevation=elevation)
            clump = str('temp_' + area + '_clump')
            grass.run_command('r.clump', input=area, output=clump, flags='d')
            value = grass.read_command('r.stats', input=clump, flags='cn', sort='desc')
            value = value.split()
            value = value[0]
            side_elev = str(elevation + '_right')
            grass.mapcalc('$outmap = if($clump == $value, $elevation, null())', outmap=side_elev, clump = clump, value = value, elevation = elevation)
            elevation = side_elev
        else: ## user set right side to analyse
            if dir == 'NS': ## side depends on flow direction
                Nw = int(waterarea.n)
                Sw = int(waterarea.s)
                Ew = int(waterarea.e)
                We = int(elevarea.w)
                grass.run_command('g.region', n=Nw, s=Sw, e=Ew, w=We, flags='a')
            elif dir == 'SN':
                Nw = int(waterarea.n)
                Sw = int(waterarea.s)
                Ee = int(elevarea.e)
                Ww = int(waterarea.w)
                grass.run_command('g.region', n=Nw, s=Sw, e=Ee, w=Ww, flags='a')
            elif dir == 'WE':
                Nw = int(waterarea.n)
                Se = int(elevarea.s)
                Ew = int(waterarea.e)
                Ww = int(waterarea.w)
                grass.run_command('g.region', n=Nw, s=Se, e=Ew, w=Ww, flags='a')
            else:
                Ne = int(elevarea.n)
                Sw = int(waterarea.s)
                Ew = int(waterarea.e)
                Ww = int(waterarea.w)
                grass.run_command('g.region', n=Ne, s=Sw, e=Ew, w=Ww, flags='a')
            area = str('temp_' + elevation + '_div')
            grass.mapcalc('$outmap = if($watercourse > 0, null(), $elevation / $elevation)', outmap=area, watercourse=watercourse, elevation=elevation)
            clump = str('temp_' + area + '_clump')
            grass.run_command('r.clump', input=area, output=clump, flags='d')
            value = grass.read_command('r.stats', input=clump, flags='cn', sort='desc')
            value = value.split()
            value = value[0]
            side_elev = str(elevation + '_right')
            grass.mapcalc('$outmap = if($clump == $value, $elevation, null())', outmap=side_elev, clump=clump, value=value, elevation=elevation)
            elevation = side_elev

### Set the area which should be exported by creating a MASK from the elevation and watercourse
    grass.run_command('g.region', rast=elevation)
    elevarea = grass.region(elevation)
    if dir == 'NS' or  'SN':
        N = int(elevarea.n)
        S = int(elevarea.s)
        E = int(elevarea.e)+2*res
        W = int(elevarea.w)-2*res
        grass.run_command('g.region', n=N, s=S, e=E, w=W, flags='a')
    if dir == 'EW' or  'WE':
        N = int(elevarea.n)+2*res
        S = int(elevarea.s)-2*res
        E = int(elevarea.e)
        W = int(elevarea.w)
        grass.run_command('g.region', n=N, s=S, e=E, w=W, flags='a')
    grass.mapcalc('MASK = if($elevation > 0 ||| $watercourse > 0, 1, null())', elevation=elevation, watercourse=watercourse)
    grass.run_command('g.region', rast='MASK')

### Prepare output and RUN R SCRIPT
    report = str(options['report'])
    if report.split('.')[-1] != 'pdf':
        grass.fatal("File type for output report is not pdf")
    terrace_map = str(options['terrace_map'])

    levels = str(options['terrlevel'])

    outputdata = os.path.join(os.path.dirname(report), 'tempout.txt')
    grass.run_command('r.stats', input=(elevation, slope, watercourse), output=outputdata, separator='space', flags='1gN') #exporting elevation, slope and watercourse altitude values, omitting cells with missing data

    grassversion = grass.version()
    grassversion = grassversion.version[:1]

    TERRargs = [elevation, slope, str(int(limit)), watercourse, dir, str(int(azimuth)), report, terrace_map, str(levels), str(grassversion)]
    pyscfold = os.path.dirname(os.path.realpath(__file__))
    pathtosc = os.path.join(pyscfold, 'TERRACE_jozsa.R')
    myRSCRIPT = [pathtor, pathtosc] + TERRargs
    if not os.path.isfile(pathtosc):
        grass.fatal("Put terrace extraction R script to GRASS scripts folder...")    

    grass.message(_("Starting R to run terrace extraction script... this may take some time..."))
    devnull = open(os.devnull, 'w')
    error = subprocess.call(myRSCRIPT, stdout=devnull, stderr=devnull)

    if error > 0:
        grass.message(_("R error log below..."))
        errorlog = os.path.join(os.path.dirname(report), 'errorlog.Rout')
        Rerror = open(errorlog, 'r')
        grass.message(_(Rerror.read()))
        Rerror.close()
        grass.fatal("Terrace extraction in R failed...")
    else:
        grass.message(_("R process finished...Continue working in GRASS GIS..."))
        
### Remove temps
    grass.run_command('g.remove', type='raster', pattern='temp_*', flags='f')
    grass.run_command('g.remove', type='raster', pattern='MASK*', flags='f')

    grass.del_temp_region()
    
if __name__ == '__main__':
    options, flags = grass.parser()
    main()
