# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

create_data_hake("lbs_to_kilos", 0.45359237)

create_data_hake("can_major_hake_areas",
                 c("03", "04", "05", "06", "07", "08", "09"))

create_data_hake("freezer_trawlers",
                 tibble::tibble(vessel = c("Viking Enterprise",
                                           "Northern Alliance",
                                           "Osprey #1",
                                           "Raw Spirit",
                                           "Pacific Legacy #1",
                                           "Sunderoey",
                                           "Viking Alliance",
                                           "Lingbank"),
                                fos_id = c(310913,
                                           312275,
                                           310988,
                                           312405,
                                           313334,
                                           313464,
                                           313224,
                                           313319),
                                gfbio_id = c(568,
                                             592,
                                             569,
                                             595,
                                             608,
                                             609,
                                             1727,
                                             1732)))
