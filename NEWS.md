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
