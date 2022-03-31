
DROP INDEX IF EXISTS public."grid_dist__sitsiteeno_distcodeGeom_idx";

CREATE INDEX IF NOT EXISTS "grid_dist__sitsiteeno_distcodeGeom_idx"
    ON public.grid_dist USING btree
    (siteno ASC NULLS LAST)
    INCLUDE(dist_code, geom)
    TABLESPACE pg_default;
	
ALTER TABLE IF EXISTS public.grid_dist
    CLUSTER ON "grid_dist__sitsiteeno_distcodeGeom_idx";
	
	
DROP INDEX IF EXISTS public."bgc__bgcid_bgc_idx";

CREATE INDEX bgc__bgcid_bgc_idx
    ON public.bgc USING btree
    (bgc_id ASC NULLS LAST)
    INCLUDE(bgc)
    TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.bgc
    CLUSTER ON "bgc__bgcid_bgc_idx";


DROP INDEX IF EXISTS public."bgc_attribution__bgcid_bgc_idx";

CREATE INDEX bgc_attribution__bgcid_bgc_idx
    ON public.bgc_attribution USING btree
    (siteno ASC NULLS LAST)
    INCLUDE(bgc)
    TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.bgc_attribution
    CLUSTER ON "bgc_attribution__bgcid_bgc_idx";
	
	
	
DROP INDEX IF EXISTS public."gcm__gcmid_gcm_idx";
	
CREATE INDEX gcm__gcmid_gcm_idx
    ON public.gcm USING btree
    (gcm_id ASC NULLS LAST)
    INCLUDE(gcm)
    TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.gcm
    CLUSTER ON "gcm__gcmid_gcm_idx";
	
	
	
DROP INDEX IF EXISTS public."futureperiod__id_futureperiod_idx";
	
CREATE INDEX futureperiod__id_futureperiod_idx
    ON public.futureperiod USING btree
    (futureperiod_id ASC NULLS LAST)
    INCLUDE(futureperiod)
    TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.futureperiod
    CLUSTER ON "futureperiod__id_futureperiod_idx";
	
	

	
	
	
DROP INDEX IF EXISTS public.hex_point__siteno_idx;

CREATE INDEX IF NOT EXISTS hex_point__siteno_idx
    ON public.hex_points USING btree
    (siteno ASC NULLS LAST)
    TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.hex_points
    CLUSTER ON hex_point__siteno_idx;
	