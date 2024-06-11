CREATE TABLE atlas_driver_offer_bpp.driver_stats ();

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN bonus_earned double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN bonus_earned_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN coin_coverted_to_cash_left double precision  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN earnings_missed double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN earnings_missed_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN idle_since timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN late_night_trips integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN rides_cancelled integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_coins_converted_cash double precision  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_distance double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_earnings double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_earnings_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_rides integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_rides_assigned integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN fav_rider_list text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN fav_rider_count integer NOT NULL;