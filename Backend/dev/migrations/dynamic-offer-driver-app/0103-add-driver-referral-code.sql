CREATE TABLE atlas_driver_offer_bpp.driver_referral (
  referral_code character varying(15) PRIMARY KEY NOT NULL,
  driver_id character varying(255) NOT NULL,
  linked_at timestamp with time zone,
 CONSTRAINT DriverReferralT_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);
