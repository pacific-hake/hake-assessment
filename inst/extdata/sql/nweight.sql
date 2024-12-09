REM 2014, the database changed in 2014
REM Wt is considered a specimen type 3
REM
REM
REM Given to Allan Hicks by Andi Stephens in 2011
REM
REM Modified to use RODBC (commented col statements)
REM Modified in 2015 for speciment type and more information
REM
REM*****************************************************************************
REM
select
 v.cruise_vessel_seq,
 t.trip_seq,
 h.CRUISE,
 h.HAUL_NUMBER,
 h.VESSEL_TYPE,
 h.CDQ_CODE,
 s.SAMPLE_NUMBER,
 s.sample_seq,
 s.total_sample_weight,
 c.species_composition_seq,
 c.species_weight,
 c.species_number,
 c.SPECIES_CODE,
 lsc.COMMON_NAME,
 l.SEX_CODE,
 l.LENGTH_SIZE,
 l.FREQUENCY,
 h.LOCATION_CODE,
 h.RETRV_LATITUDE_DEGREES,
 h.RETRV_LATITUDE_MINUTES,
 h.RETRV_LATITUDE_SECONDS,
 h.RETRV_EW,
 h.RETRV_LONGITUDE_DEGREES,
 h.RETRV_LONGITUDE_MINUTES,
 h.RETRV_LONGITUDE_SECONDS,
 h.BOTTOM_DEPTH,
 h.FISHING_DEPTH,
 h.DEPTH_METER_FATHOM,
 h.retrv_date_time,
 h.deploy_date_time,
 h.obsvr_est_catch,
 h.vessel_est_catch,
 h.haul_seq,
 fis.WEIGHT,
 fis.AGE,
 fis.SPECIMEN_NUMBER,
 fis.SPECIMEN_TYPE,
 fis.BARCODE
from
 norpac.ATL_CRUISE_VESSEL v,
 norpac.ATL_FMA_TRIP t,
 norpac.ATL_HAUL h,
 norpac.ATL_SAMPLE s,
 norpac.ATL_SPECIES_COMPOSITION c,
 norpac.ATL_LOV_SPECIES_CODE lsc,
 norpac.ATL_LENGTH l
 full join norpac.ATL_FISH_INV_SPECIMEN fis
 ON l.CRUISE = fis.CRUISE AND l.LENGTH_SEQ = fis.LENGTH_SEQ
where
 v.cruise = t.cruise and
 v.cruise = h.cruise and
 v.cruise_vessel_seq = t.cruise_vessel_seq and
 t.cruise = h.cruise and
 t.trip_seq = h.trip_seq and
 s.CRUISE = c.CRUISE AND
 s.SAMPLE_SEQ = c.SAMPLE_SEQ and
 c.CRUISE = l.CRUISE AND
 c.SPECIES_COMPOSITION_SEQ = l.SPECIES_COMPOSITION_SEQ and
 lsc.SPECIES_CODE = c.SPECIES_CODE and
 h.CRUISE = s.CRUISE and
 h.HAUL_SEQ = s.HAUL_SEQ and
 c.SPECIES_CODE = &sp and
 h.RETRV_DATE_TIME >= to_date('01-01-&beginyr','dd-mm-yyyy') and
 h.RETRV_DATE_TIME <= to_date('31-12-&endyr','dd-mm-yyyy') and
 h.RETRV_LATITUDE_DEGREES < 49 and
 fis.SPECIMEN_TYPE = 3
 ;
