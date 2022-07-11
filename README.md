# GeoAnomalia. 

### Summary: 

This software offers possibility to conduct 3D geometric analysis (3D outlier detection) for geological terrains.

For data used in our article, see our Zenodo repository: https://doi.org/10.5281/zenodo.6405496

A freely available C++ code (orientation_maps.cpp - https://github.com/michalmichalak997/SurfaceCompare/blob/master/orientation_maps.cpp) is required to produce files for the spatial portion of the research.
However, to run orientation.cpp file (https://github.com/michalmichalak997/SurfaceCompare/blob/master/orientation_maps.cpp), 
you will need first to install CGAL library (https://doc.cgal.org/latest/Manual/installation.html).

The C++ program (orientation_maps.cpp - https://github.com/michalmichalak997/SurfaceCompare/blob/master/orientation_maps.cpp) requires a text file as input with the following columns: 
XY coordinates common for all horizons as two first columns, n columns representing n horizons and the id pointing to the row number as the last column. 

If you want to process our data, you will need two .R files producing spatial outputs: for Kraków-Silesian Homocline (with grid maps), and Central European Basin System, respectively. 

Developer: Michał Michalak (michalmichalak@us.edu.pl)

Affiliations: 
1) Faculty of Natural Sciences, University of Silesia in Katowice, Poland, Będzińska 60, 41-205 Sosnowiec.
2) Faculty of Geology, Geophysics and Environmental Protection, Poland, aleja Adama Mickiewicza 30, 30-059 Kraków.


## Input

See our Zenodo repository to download the files used in our article (https://doi.org/10.5281/zenodo.6405496).
For Kraków Silesian Homocline you will need the following file as input: KSH_input.txt.
For Central European Basin System you will need the following file as input: CEBS_input.txt.

For your own data, you will need to prepare your input files in the following format (the data frame has 41 observations - surface points):  

0.4 2.0 4.5 1  
0.2 0.1 10.3 2  
...  
...  
...  
0.12 10.1 14.1 41  

Where the columns denote the following properties:  
1st column: latitude in UTM  
2nd column: longitude in UTM  
3rd column: elevation  
4th column: id  

In fact, the orientation.cpp file accepts many elevations, so the below structure is accepted as well:  

0.4 2.0 4.5 1.1 2.1 ... 1  
0.2 0.1 10.3 0.6 1.1 .... 2  
...  
...  
...  
0.12 10.1 14.1 9.1 10.1 .... 41  


1st column: latitude in UTM  
2nd column: longitude in UTM  
3rd column: elevation 1  
4th column: elevation 2  
5th column: elevation 3  
...  
...  
...  
Last column: id  



## Processing and output: Orientation maps

To create clustering and grid maps used in the article you need to first use the orientation_maps.cpp file. The structure of the input is presented above. 
Note that you need to specify the spacing of the regular grid. 
To obtain the grid map, you will specifically need the file with the extension _grid_locate.

Please note that in the below example the spacing between points in the regular grid will be 150 m.

![processing_gridmaps](https://user-images.githubusercontent.com/28152295/161153466-fa068793-141a-46d4-939c-a58914b2d853.png)

## System requirements

### System required

Windows 10 Home, 64-bit, processor x64.

### Versions the software has been tested on

Windows 10 Home, 64-bit, processor x64.

### Software required 

Version numbers can be different, those used in our work are given: 

  -Microsoft Visual Studio 2017, 
  
  -CGAL library (ver. 4.8),
  
  -Boost library (ver. 1.59), 
  
  -Microsoft Visual C++ 2017 Redistributable
  
### Program language

C++

### License

GNU General Public License v3.0.


# GeoAnomalia. Move to R - part 1 - Spatial analyses, clustering, cluster centres.

You should first delete the first line in your file that includes information about the taken horizons. 
For example, in the file with extension _01 you should delete the first line "Index of the first surface:0Index of the first surface:1".

Use the following R scripts:

KSH.R (for Kraków-Silesian Homocline). You will need the following files from our Zenodo repo: 

KSH_output.txt
KSH_grid_locate_200.txt
KSH_grid_locate_150.txt
KSH_grid_locate_100.txt
KSH_grid_locate_50.txt

CEBS.R (for Central European Basin System). You will need the following files from our Zenodo repo: 

CEBS_output.txt


