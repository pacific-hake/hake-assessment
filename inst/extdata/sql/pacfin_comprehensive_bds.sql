select
   sample_year,
   sample_month,
   sample_agency,
   SAMPLE_METHOD_CODE sample_method,
   sample_type,
   agency_code source_agid,
   SAMPLE_NUMBER sample_no,
   CLUSTER_SEQUENCE_NUMBER cluster_no,
   CLUSTER_WEIGHT_LBS cluster_wgt,
   SPECIES_WEIGHT_LBS species_wgt,
   AGENCY_PORT_CODE port,
   AGENCY_GEAR_CODE gear,
   PACFIN_GEAR_CODE grid,
   data_type,
   FISH_LENGTH,
   FISH_LENGTH_UNITS,
   FISH_MATURITY_CODE maturity,
   FISH_WEIGHT,
   FISH_WEIGHT_UNITS,
   SEX_CODE sex,
   WEIGHT_OF_MALES_LBS males_wgt,
   NUMBER_OF_MALES males_num,
   WEIGHT_OF_FEMALES_LBS females_wgt,
   NUMBER_OF_FEMALES females_num,
   WEIGHT_OF_LANDING_LBS total_wgt,
   EXPANDED_SAMPLE_WEIGHT exp_wt,
   FINAL_FISH_AGE_IN_YEARS AGE,
   OBSERVED_FREQUENCY FREQ
FROM pacfin_marts.COMPREHENSIVE_BDS_COMM
WHERE
   PACFIN_SPECIES_CODE LIKE upper(&sp) AND
   (AGE_SEQUENCE_NUMBER = 1 OR AGE_SEQUENCE_NUMBER is null) AND
   sample_year between &beginyr and &endyr
ORDER BY
   sample_year,
   sample_agency;
