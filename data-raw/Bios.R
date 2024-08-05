## code to prepare `Bios` dataset goes here


Bios <- terra::rast("data-raw/Bios.tif")

Bios <- terra::wrap(Bios)

usethis::use_data(Bios, overwrite = TRUE)
