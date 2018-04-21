# NooksackWaterBudget
Water data and models to support water resource professionals and their salmon recovery partners working with the WRIA 1 Joint Board on water supply planning and instream flow negotiations.

Run make to compile in UNIX.

On April 21st a problem with coordinate conversion that affected
processes such as evapotranspiration was fixed that resulted in
adding a dependency for the Proj4 geospatial library (-lproj).
The problem was due to a linear conversion using parameters from an
input file, but the parameters for the input file available in
the input data set were for the wrong UTM zone. The effect was
that Whatom County positons were converted to positions East of
the Cascades, and those positions were then used in subsequent
calculations by Topnet.

The fix could have been to figure out the correct parameters for
the input file, but simply writing code to make the conversion
in TopNet itself was more accurate and reliable.
