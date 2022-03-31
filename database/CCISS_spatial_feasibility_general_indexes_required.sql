	
DROP INDEX IF EXISTS public."grid_dist__gismGEOG_idx";
	
CREATE INDEX "grid_dist__gismGEOG_idx"
    ON public.grid_dist USING gist
    (geom)
    TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.grid_dist
    CLUSTER ON "grid_dist__gismGEOG_idx";