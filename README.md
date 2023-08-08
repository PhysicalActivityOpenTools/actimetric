
# actimetric

<!-- badges: start -->
<!-- badges: end -->

The goal of actimetric is to apply machine learning-based algorithms to 
accelerometer data to provide insights in physical activity and sleep
behaviors in free-living conditions.

## Installation

You can install the development version of actimetric like so:

``` r
library(remotes)
install_github("jhmigueles/actimetric")
```

## Example usage

To use actimetric, you just need to follow the next steps:

``` r
library(actimetric)

# Select the processing routine from the options available

dothis  = choice() 
# this gives the user the option to select algorithm, acceleration metric
# and other features from a list of available options


# This runs the program
features(output = "G:/directory_to_save/the_output_files/",
         Calibrate = TRUE,
         visual = TRUE,
         Guide = F,
         sleep = F,
         dothis = dothis,
         folder_name ="_myStudy",
         file_name="_myStudy")

```

