# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_analysis_1.R")
source(file = "R/04_analysis_2.R")
source(file = "R/04_analysis_3.R")
source(file = "R/04_analysis_4.R")
source(file = "R/04_analysis_5.R")
source(file = "R/04_analysis_6.R")

# Knit the presentation ---------------------------------------------------
rmarkdown::render("doc/presentation.Rmd")
