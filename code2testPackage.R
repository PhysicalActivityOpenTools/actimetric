
library(actimetric)

c = 1

classify(input_directory = "D:/GGIRdev/files/nonwear/smallfile/",
         output_directory = "D:/actimetric/",
         studyname = c("PS_wrist", "PS_hip", "PS_hip_LL", "PS_wrist_LL",
                       "School_wrist", "School_hip", "Adult_wrist_trost",
                       "Adult_women_wrist_ellis", "Adult_women_hip_ellis",
                       "thigh")[c],
         classifier = c("Preschool Wrist Random Forest Free Living",
                        "Preschool Hip Random Forest Free Living",
                        "Preschool Hip Random Forest Free Living Lag-Lead",
                        "Preschool Wrist Random Forest Free Living Lag-Lead",
                        "School age Wrist Random Forest",
                        "School age Hip Random Forest",
                        "Adult Wrist RF Trost",
                        "Adult women Wrist RF Ellis",
                        "Adult women Hip RF Ellis",
                        "Thigh Decision Tree")[c],
         do.calibration = TRUE,
         do.sleep = TRUE,
         do.nonwear = TRUE,
         do.enmo = TRUE,
         do.actilifecounts = FALSE,
         do.actilifecountsLFE = FALSE,
         boutdur = 10, boutcriter = 0.8,
         verbose = TRUE, overwrite = FALSE)


# interactive selection of directories: both interactive and via code
# reading ax3 and ax6
# sens data
# sleep algorithms with counts
