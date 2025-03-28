SELECT
  DEBRIEFED_AGE_SQUASH_SP_TYPE.HAUL_JOIN,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.AGE,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.LENGTH,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.WEIGHT,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.SEX,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.CRUISE,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.PERMIT,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.CDQ_CODE,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.BOTTOM_DEPTH_FATHOMS,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.FISHING_DEPTH_FATHOMS,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.HAUL_OFFLOAD,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.HAUL_OFFLOAD_DATE,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.LATDD_START,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.LATDD_END,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.LONDD_START,
  DEBRIEFED_AGE_SQUASH_SP_TYPE.LONDD_END
FROM DEBRIEFED_AGE_SQUASH_SP_TYPE
WHERE DEBRIEFED_AGE_SQUASH_SP_TYPE.SPECIES = &sp
AND DEBRIEFED_AGE_SQUASH_SP_TYPE.YEAR >= &beginyr
AND DEBRIEFED_AGE_SQUASH_SP_TYPE.YEAR <= &endyr;
