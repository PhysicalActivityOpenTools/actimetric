# actimetric 0.1.6

* Now calibration algorithm is only applied if it improves the original error in the data. #71 

# actimetric 0.1.5

* Time zone consideration for building the time stamps (new argument: tz). #69
* Gaps in raw data longer than 90 minutes are now imputed once the data have been aggregated. #69
* Small fixes to warning messaging related to sleep detection using data collected on hip. 

# actimetric 0.1.4

* Time series: 
    - stores classes as factor in time series to ease interpretation #51
    - fixed minor bug related to assignation of date to sleep periods #60
* Documentation: minor revision of documentation for functions check_classifier and getBout #41
* Reading data: minor revision of ReadAndCalibrate when reading ActiGraph devices with the Idle Sleep Mode activated #58
* Feature extraction: numeric features are no longer rounded to 3 decimal places #61
* Calibration: trycatch function applied to calibration routine for cases in which coefficients cannot be retrieved #63
* Aggregation per date: fixed minor bug that broke data aggregation in the last sleep period when recording ends before noon #65

# actimetric 0.1.3

* Calibration: fix minor bug by which calibration procedure was using time column instead of acceleration columns in data #49

# actimetric 0.1.2

* Documentation: include references to papers, other pieces of code, packages, and authorships #41

# actimetric 0.1.1

* Data reading: fix minor bug when reading short files #25
* Data reading: filter input files so that package only loads gt3x, bin, or cwa files #29
* Data imputation: fix bug occurring when reading ActiGraph files with idle sleep mode #32
* Pipeline: calibration process now occurs earlier in the pipeline, right after data reading #33
* Non wear: fix minor bug that triggered an error when full chunk of data is non wear #35
* Reports: fix minor bug that produced an error if sleep is not calculated but nonwear is #26
* Reports: replace tryCatch statements in aggregate_per_person #37
* Bouts: Implemented functionality that allows users to define gaps in bout as a percentage of boutduration or as an absolute number of minutes, and it also allows for the definition of the max gap length #28

# actimetric 0.1.0

* First GitHub release.
