1. Software

Install R and RStudio, then run this once in the R console:

install.packages(c("readxl", "pROC", "ggplot2", "rms", "openxlsx"))

2. File locations

Place the files as follows, using these paths relative to that folder:

analys.R
README.txt
EPSONIP_DATA_xxxx.csv
EPSONIP_xxxx.xlsx

Participant data are not included in this code package.

3. Run in RStudio

source("analys.R")
