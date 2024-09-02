# actimetric 0.1.4

* Time series: stores classes as factor in time series to ease interpretation #51
* Documentation: minor revision of documentation for functions check_classifier and getBout #41
* Reading data: minor revision of ReadAndCalibrate when reading ActiGraph devices with the Idle Sleep Mode activated #58

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
