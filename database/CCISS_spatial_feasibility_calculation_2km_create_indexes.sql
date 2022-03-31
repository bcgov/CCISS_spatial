DROP INDEX IF EXISTS pts2km_feas__fpspeda_sicurrnews_idx;

CREATE INDEX IF NOT EXISTS pts2km_feas__fpspeda_sicurrnews_idx
    ON pts2km_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default;

ALTER TABLE IF EXISTS pts2km_feas
    CLUSTER ON pts2km_feas__fpspeda_sicurrnews_idx;