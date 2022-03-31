
-- Indexes the pts400m_feas for quick execution.   The index format is in FPxxx_SPyyyy format
--      FP = FutuerPeriod ID filter; 1 through 5
--      SP = Species ID filter based on Less than 19; Between 19 and 37; Between 38 and 55; or, Over 55

DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP1_SPLT19;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP1_SPBT19n37;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP1_SPBT38n55;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP1_SPGT55;
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP1_SPLT19
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=1) and (species_id<19);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP1_SPBT19n37
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=1) and (species_id between 19 and 37);

CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP1_SPBT38n55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=1) and (species_id between 38 and 55);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP1_SPGT55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=1) and (species_id>55);


DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP2_SPLT19;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP2_SPBT19n37;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP2_SPBT38n55;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP2_SPGT55;
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP2_SPLT19
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=2) and (species_id<19);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP2_SPBT19n37
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=2) and (species_id between 19 and 37);

CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP2_SPBT38n55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=2) and (species_id between 38 and 55);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP2_SPGT55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=2) and (species_id>55);
	
	
	
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP3_SPLT19;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP3_SPBT19n37;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP3_SPBT38n55;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP3_SPGT55;
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP3_SPLT19
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=3) and (species_id<19);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP3_SPBT19n37
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=3) and (species_id between 19 and 37);

CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP3_SPBT38n55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=3) and (species_id between 38 and 55);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP3_SPGT55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=3) and (species_id>55);
	
	
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP4_SPLT19;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP4_SPBT19n37;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP4_SPBT38n55;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP4_SPGT55;
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP4_SPLT19
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=4) and (species_id<19);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP4_SPBT19n37
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=4) and (species_id between 19 and 37);

CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP4_SPBT38n55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=4) and (species_id between 38 and 55);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP4_SPGT55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=4) and (species_id>55);
	
	
	
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP5_SPLT19;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP5_SPBT19n37;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP5_SPBT38n55;
DROP INDEX IF EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP5_SPGT55;
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP5_SPLT19
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=5) and (species_id<19);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP5_SPBT19n37
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=5) and (species_id between 19 and 37);

CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP5_SPBT38n55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=5) and (species_id between 38 and 55);
	
CREATE INDEX IF NOT EXISTS pts400m_feas__fpspeda_sicurrnews_idx_FP5_SPGT55
    ON pts400m_feas USING btree
    (futureperiod_id ASC NULLS LAST, species_id ASC NULLS LAST, edatope_id ASC NULLS LAST)
    INCLUDE(siteno, curr, newsuit)
    TABLESPACE pg_default 
	WHERE (futureperiod_id=5) and (species_id>55);
