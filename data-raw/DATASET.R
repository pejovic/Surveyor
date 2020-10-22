## code to prepare `DATASET` dataset goes here

library(Surveyor)

snet2D <- read_surveynet(file = here::here("inst/extdata/snet2D.xlsx"), dest_crs = 3909)
snet2D.adjust <- adjust.snet(adjust = TRUE, survey.net = snet2D, dim_type = "2D", sd.apriori = 1 ,  all = FALSE, ellipse.scale = 10)


usethis::use_data(snet2D, overwrite = TRUE)

snet1D_epoch_1 <- read_surveynet(file = here::here("inst/extdata/snet1D_epoch_1.xlsx"), dest_crs = NA)

usethis::use_data(snet1D_epoch_1, overwrite = TRUE)


snet1D_epoch_2 <- read_surveynet(file = here::here("inst/extdata/snet1D_epoch_2.xlsx"), dest_crs = NA)

usethis::use_data(snet1D_epoch_2, overwrite = TRUE)

epoch1 = "inst/extdata/snet1D_epoch_1.xlsx"
snet1D_epoch_1 <- read_surveynet(file = here::here("inst/extdata/snet1D_epoch_1.xlsx"), dest_crs = NA)
epoch2 = "inst/extdata/snet1D_epoch_2.xlsx"
snet1D_epoch_2 <- read_surveynet(file = here::here("inst/extdata/snet1D_epoch_2.xlsx"), dest_crs = NA)

epoch1.adjust <- adjust.snet(adjust = TRUE, survey.net = snet1D_epoch_1, dim_type = "1D", sd.apriori = 0.5 ,  all = FALSE)
epoch2.adjust <- adjust.snet(adjust = TRUE, survey.net = snet1D_epoch_2, dim_type = "1D", sd.apriori = 0.5 ,  all = FALSE)

aa <- deform.snet(snet_1_path = epoch1, snet_2_path = epoch2, sd.apriori = 0.5, units = "mm",  dim_type = "1D", prob = 0.95, wdh_model = "n_dh")

