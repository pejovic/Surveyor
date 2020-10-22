
library(Surveyor)

snet2D <- read_surveynet(file = here::here("inst/extdata/snet2D.xlsx"), dest_crs = 3909)

usethis::use_data(snet2D, overwrite = TRUE)

snet1D_epoch_1 <- read_surveynet(file = here::here("inst/extdata/snet1D_epoch_1.xlsx"), dest_crs = NA)

usethis::use_data(snet1D_epoch_1, overwrite = TRUE)


snet1D_epoch_2 <- read_surveynet(file = here::here("inst/extdata/snet1D_epoch_2.xlsx"), dest_crs = NA)

usethis::use_data(snet1D_epoch_2, overwrite = TRUE)
