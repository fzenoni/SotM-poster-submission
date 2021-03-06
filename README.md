---
output:
  html_document: default
  pdf_document: default
---
# State of the Map poster submission
_by Florian Zenoni_

![](FZenoni_SotM_Poster.jpg)

## Summary
The following poster selected at the 2018 edition of the State of the Map (SotM) conference includes two maps that are generated by a series of scripts. The computational part is done thanks to a local instance of Open Source Routing Machine (OSRM), called thanks to a script that queries the C++ library API. The analysis of the raw data as well as the manipulation of the spatial objects are taken care of by a set of scripts written in the R language.

## What is included and what is not
This GitHub repository includes all of the aforementioned scripts.
What is not included is the local OSRM backup instance files, as well as the OpenStreetMap (OSM) map of the Northwestern Italian regions used by the routing engine. Therefore, to fully reproduce the workflow, you will first need to set-up the OSRM backend accordingly. This README does not cover this part. All information can be found at the [official OSRM Wiki](https://github.com/Project-OSRM/osrm-backend/wiki). The rest of this README assumes you this installation was successful, and that you are able to execute the `osrm-routed` command.

## How to reproduce the poster
### Build the map
We rely on the [GeoFabrik.de](https://www.geofabrik.de) website to download the OpenStreetMap data relevant to the area cover by the poster. Unfortunately the smallest available file covers several regions in Northwestern Italy, which are obviously far larger that the city of Milan. This is a problem only for the preparation step, at it will take you more time than effectively necessary to build the files for OSRM. Anyways, this map is available at the following [link](http://download.geofabrik.de/europe/italy/nord-ovest.html). You can download the `.osm.pbf` file in a folder parallel to the current one, for instance `../map`.
I followed the steps shown by [this tutorial](https://github.com/Project-OSRM/osrm-backend/wiki/Running-OSRM) to prepare the map with the Contraction Hierarchies pipeline. The only difference is the usage of the `foot.lua` profile, instead of `car.lua`, to compute distances by walking.

### Download the Urban Atlas
The polygons on the poster are infact urban building blocks, coming from the [Urban Atlas 2012 database](https://land.copernicus.eu/local/urban-atlas/urban-atlas-2012). Many cities and urban areas are available, but to reproduce the poster you will of course need to pick the one relative to Milan. Download it and unzip the folder into `Rscripts/data`.

### Execute the first part of the R code
The script `Milan_harvest.R` is the one in charge of most of the computations and manipulation performed for the poster. The other files contain custom functions to be used in that script.  Its execution is pretty straightforward. Around line 90, tough, you will need to compute all the distances, from polygon to restaurant. To do so, please refer to the next section.

### Compile and execute the C++ script
The script is a modification of the example provided with the OSRM source code, available [here](https://github.com/Project-OSRM/osrm-backend/tree/master/example). This code will query the OSRM C++ API thanks to the `libosrm` library. Therefore, there is no need for this script to run the HTTP API.
To build the script and run the script:

1. open a terminal UNIX and move to the `osrm-application/build/` folder,
1. run `cmake ..`,
1. run `make -j`,
1. eventually run the application in the following form: `./application input_file.txt map.osrm`. Replace the two arguments according to their relative or absolute path. `input_file.txt` is the list of origins and destinations generated by the first part of the `milan_harvest.R` script, while `map.osrm` is the map builded by the OSRM commands, explained above.

Depending on your machine and how many cores it has, the computation shouldn't take too long. The script will generate a file in the `data/` folder, called `osrm-output.csv`.

### Execute the second part of the R code
Now that `osrm-output.csv` has been generated, the second part of the code can be freely run without any other preparation step. This part will plot the large picture of the poster.

### Create the smaller picture
The detail of a Milan area with superimposed tracks leading to different destinations is generated by the `R-scripts/routing.R` script. This code makes use of the HTTP API of OSRM. Therefore, you will need to move to the folder where you map has been built (in our case it was a parallel folder called `map/`) and execute the following in a terminal: `osrm-routed map.osrm`, where `map.osrm` is the name of the osrm object (if you didn't rename it, it is probably `nord-ovest-latest.osrm`.