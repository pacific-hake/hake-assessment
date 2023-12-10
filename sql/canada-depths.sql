select
c.FISHING_EVENT_ID as fishing_event_id,
c.BEST_DATE as best_date,
c.VESSEL_REGISTRATION_NUMBER as vessel_id,
c.MAJOR_STAT_AREA_CODE as major_stat_area_code,
c.BEST_DEPTH_FM as bottom_depth_fm,
ts.GEAR_DEPTH_FM as gear_depth_fm

from GF_D_OFFICIAL_FE_CATCH c
left join GF_FE_TRAWL_SPECS ts on c.FISHING_EVENT_ID = ts.FISHING_EVENT_ID
where
c.FISHERY_SECTOR = 'GROUNDFISH TRAWL' and
-- inject fishery categories here
c.SPECIES_CODE = '225'
group by
c.FISHING_EVENT_ID,
c.BEST_DATE,
c.VESSEL_REGISTRATION_NUMBER,
c.BEST_RETAINED_ROUND_KG,
c.MAJOR_STAT_AREA_CODE,
c.BEST_DEPTH_FM,
ts.GEAR_DEPTH_FM
having
-- inject vessel codes here
c.BEST_RETAINED_ROUND_KG is not null
order by
c.BEST_DATE desc
