ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN exo_phones text[] ;
ALTER TABLE atlas_driver_offer_bpp.merchant ADD COLUMN exo_phone_country_code character varying(36) ;
UPDATE atlas_driver_offer_bpp.merchant SET exo_phones = '{"8069457995","8035272983"}', exo_phone_country_code = '+91';
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN exo_phones SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN exo_phone_country_code SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id) default 'favorit0-0000-0000-0000-00000favorit';