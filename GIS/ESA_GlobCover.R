#R Version 3.4.4
#Playing with land cover data from the ESA's GlobCover project (http://due.esrin.esa.int/page_globcover.php)
require("raster")
require("rworldmap")
require("rworldxtra")

globcover <- raster("~/Downloads/GLOBCOVER_L4_200901_200912_V2.3.tif")

plot(globcover)
