# Occupancy
This repository contains research into using tools written in the R programming language to model dynamic occupancy based on camera trap detection data. Occupancy is defined as the probability that a particular site will contain a particular entity (i.e: an animal species in an ecological study), and it is made "dynamic" or "multi-season" by modeling occupancy (and its changes) over time.

---

### Our Data
Our research data concerns camera trap studies performed on mammals in the lowland and cloud forests of Costa Rica.

---

### Files in the Repository
The repository's root folder currently has three files of interest:

- **Archive**: This folder contains old scripts and R programs. It is not maintained, but it may be useful as a reference when writing new scripts involving the R packages `unmarked` or `proj4`
- **experiment.R**: This script is currently the site of active development. It's the sandbox where new functionality is tested before being formally added to more permanent programs.
- **GIS**: This folder contains the raster data sets we're using and some scripts for importing that data into R. It may be a useful reference when working with the R package `raster`.

---

### License
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation: either version 3 of the License, or any later version.
