
drop table if exists pts2km_feas;


CREATE TABLE IF NOT EXISTS public.pts2km_feas
(
    siteno integer,
	futureperiod_id smallint,
	species_id smallint,
	edatope_id smallint,
    curr numeric,
    newsuit numeric
);

drop table if exists pts400m_feas;


CREATE TABLE IF NOT EXISTS public.pts400m_feas
(
    siteno integer,
	futureperiod_id smallint,
	species_id smallint,
	edatope_id smallint,
    curr numeric,
    newsuit numeric 
);


drop table if exists species;


CREATE TABLE IF NOT EXISTS species
(
    species_id smallint	PRIMARY KEY,
	species text
);


drop table if exists edatope;


CREATE TABLE IF NOT EXISTS edatope
(
    edatope_id smallint PRIMARY KEY,
    edatope text 
)
