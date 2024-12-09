# Create internal package data
fathom_to_meter <- 1.8288
pound_to_gram <- 453.5924
species_norpac <- 206

usethis::use_data(
  fathom_to_meter,
  pound_to_gram,
  species_norpac,
  internal = TRUE,
  overwrite = TRUE
)
rm(fathom_to_meter, pound_to_gram, species_norpac)
