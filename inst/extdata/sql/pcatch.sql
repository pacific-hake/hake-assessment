REM Get pacfin catch from PACFIN_MARTS.COMPREHENSIVE_FT table, which includes research catch
SELECT   cft.PACFIN_YEAR YEAR,
         cft.FLEET_CODE FLEET,
         cft.PACFIN_GEAR_CODE GRID,
         cft.LANDING_DATE,
         cft.VESSEL_ID,
         SUM(landed_weight_lbs) AS LBS,
         SUM(landed_weight_mtons) as MT,
         SUM(round_weight_mtons) as rMT
FROM    pacfin_marts.comprehensive_ft cft 
WHERE
        cft.PACFIN_YEAR >= &beginyr AND
        cft.PACFIN_YEAR <= &endyr AND
        cft.PACFIN_SPECIES_CODE = upper(&sp) AND
        cft.COUNCIL_CODE = 'P'
 GROUP BY  cft.PACFIN_YEAR,
           cft.FLEET_CODE,
           cft.PACFIN_GEAR_CODE,
           cft.LANDING_DATE,
           cft.VESSEL_ID,
           cft.PACFIN_PORT_CODE,
           cft.DEALER_NUM,
           cft.DAHL_GROUNDFISH_CODE;
