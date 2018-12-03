# GetNuts

**GetNuts** is a pipeline that takes data tables (`.xlsx`) pulled from Pulsar and can output a series of static plots as well as an interactive shiny dashboard. This pipeline is tested to work on Windows, macOS and Linux.

To use **GetNuts**, download or clone the repo to a folder on your computer, then create a folder `/Data` where pulsar `.xlsx` data can be placed. **Important:** if using multiple excel files, it is important that only the first file has header information in the table.

**GetNuts** analysis can be modularized by selecting the desired outputs in config files, which enables separation of project and analysis.  An example config file can be seen in the `/config_files` directory.

## Installation

**Dependencies:**

* [readxl](https://github.com/tidyverse/readxl)
* [quanteda](https://github.com/quanteda/quanteda)
* [magrittr](https://github.com/tidyverse/magrittr)
* [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)
* [ggplot2](https://ggplot2.tidyverse.org/)
* [leaflet](https://rstudio.github.io/leaflet/)
* [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html)
* [sp](https://cran.r-project.org/web/packages/sp/index.html)
* [spdplyr](https://cran.r-project.org/web/packages/spdplyr/index.html)
* [ggmap](https://github.com/dkahle/ggmap)
* [rgeos](https://cran.r-project.org/web/packages/rgeos/index.html)
* [maptools](https://cran.r-project.org/web/packages/maptools/index.html)

**Prior to running the Pipeline install:**

```r
install.packages(c(
  "readxl","stringi","quanteda","magrittr",
  "tidyr","ggplot2","ggpubr","shiny","leaflet",
  "sp","rgdal","spdplyr","rgeos","maptools"))
```

and

The latest ggmap must be installed from github:

```r
if(!requireNamespace("devtools"))
install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
```

Then, clone or download the **GetNuts** repository to your local computer.

## Configuration

To analyze data, create a folder `/Data` in the repo parent folder and place Pulsar `.xlsx` data there.

To perform an analysis, the user must specify a `config.txt` file to run with GetNuts. Config files are stored in `/config_files` and specify parameters of the analysis to be performed. These include:

* `input_file_dir:` where the data has been stored (Default is `Data`)
* `input_file_list:` The files in the `input_file_dir` to be analyzed. **Important:** the first file should have the header information, all files after should not have header information
* `sheet_name:` sheet of the excel file to take the data from (Default is `Sheet1`)
* `project_name:` Name given to analysis. This will create a folder of this name in `/output_files/` where all output from this analysis will be found.
* `rerun_data_preprocessing:` ("yes" or "no") Data Preprocessing, which involves stopword removal and stemming is the most computationally expensive process in the pipeline. If set to "yes", pipeline will be re-run from the beginning. (Default: "no")
* `rerun_data_labelling:` ("yes" or "no") Data labelling refers to when the preprocessed data is checked against the supplied dictionaries (in `/Dictionaries`). If a change is made to a dictionary, the analysis does not need to be re-run from the beginning, it can begin here if set to "yes". (Default: no)

For the outputs, the user can also specify in the config file what is desired and in which format.
* `do_static_plots:` ("yes" or "no") Specify whether to export static plot files (Default: yes)
* `static_plot_format:` Desired output format for output plots. Can be any one of `png`,`jpeg`,`pdf`,`eps`,`svg`,`tif`,`bmp`. (no default)
* `launch_shiny_dash_board:` ("yes" or no") To launch shiny dash board change this option to "yes".


## Analysis

To run the analysis, open an R console and

* Navigate to the GetNuts Directory

```r
setwd("your/GetNuts/Directory")
```

* Source the Main script:

```r
source("GetNuts.R")
```

* Call the function `allergies_and_social_media` with the desired config file:

```r
allergies_and_social_media("config_files/your_config_file")
```

Which will run the entire pipeline with the configuration supplied.

* Check `output_files/$project_name/` for all outputs from the analysis.
