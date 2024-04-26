-- NOTE : THIS QUERY IS ONLY FOR LOCAL DO NOT RUN IN MASTER
UPDATE atlas_driver_offer_bpp.merchant_overlay
SET merchant_id = 'favorit0-0000-0000-0000-00000favorit'
WHERE merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- ADDING TABLES FOR MERCHANT OPERATING CITY
CREATE TABLE atlas_driver_offer_bpp.merchant_operating_city (
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    merchant_short_id character varying(255) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (short_id),
    city character varying(255) NOT NULL
);

-- INSERT INTO atlas_driver_offer_bpp.merchant_operating_city (id, merchant_id, merchant_short_id, city)
-- SELECT
--     atlas_driver_offer_bpp.uuid_generate_v4() AS id,
--     id AS merchant_id,
--     short_id AS merchant_short_id,
--     city
-- FROM atlas_driver_offer_bpp.merchant;

-- for local testing only
INSERT INTO atlas_driver_offer_bpp.merchant_operating_city (id, merchant_id, merchant_short_id, city) VALUES
('favorit0-0000-0000-0000-00000000city', 'favorit0-0000-0000-0000-00000favorit', 'NAMMA_YATRI_PARTNER', 'Kochi'),
('nearest-drivers-testing-org00000city', 'nearest-drivers-testing-organization', 'OTHER_MERCHANT_2', 'Kochi');

-- ADDING MERCHANT_OPERATING_CITY_ID TO EXISTING TABLES
ALTER TABLE atlas_driver_offer_bpp.booking
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.go_home_config
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.person
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.registration_token
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

-- for local testing only
UPDATE atlas_driver_offer_bpp.registration_token SET merchant_operating_city_id = 'favorit0-0000-0000-0000-00000000city';

ALTER TABLE atlas_driver_offer_bpp.search_request
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.search_try
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.merchant_message
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.merchant_payment_method
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.merchant_overlay
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

-- UPDATING VALUES OF MERCHANT_OPERATING_CITY_ID FOR CONFIG TABLES
UPDATE atlas_driver_offer_bpp.go_home_config
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.go_home_config.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.merchant_service_usage_config.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.driver_intelligent_pool_config
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.driver_intelligent_pool_config.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.merchant_message
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.merchant_message.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.merchant_payment_method
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.merchant_payment_method.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.onboarding_document_configs
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.onboarding_document_configs.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.merchant_overlay
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.merchant_overlay.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.transporter_config
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.transporter_config.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.exophone
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.exophone.merchant_id = merchant_operating_city.merchant_id;

-- -- SETTING COLUMN AS NOT NULL
ALTER TABLE atlas_driver_offer_bpp.go_home_config
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config
ALTER COLUMN merchant_operating_city_id SET NOT NULL;


ALTER TABLE atlas_driver_offer_bpp.merchant_message
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.merchant_payment_method
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.merchant_overlay
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.exophone
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

-- DROPPING PRIMARY KEY CONSTRAINTS
ALTER TABLE atlas_driver_offer_bpp.go_home_config
DROP CONSTRAINT go_home_config_pkey;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
DROP CONSTRAINT merchant_service_usage_config_pkey;

ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config
DROP CONSTRAINT driver_intelligent_pool_config_pkey;

ALTER TABLE atlas_driver_offer_bpp.merchant_message
DROP CONSTRAINT merchant_message_pkey;

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
DROP CONSTRAINT PK_onboarding_document_configs;

ALTER TABLE atlas_driver_offer_bpp.transporter_config
DROP CONSTRAINT transporter_config_pkey;

-- ADDING MERCHANT_OPERATING_CITY_ID AS PRIMARY KEY
ALTER TABLE atlas_driver_offer_bpp.go_home_config
ADD PRIMARY KEY (merchant_operating_city_id);

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
ADD PRIMARY KEY (merchant_operating_city_id);

ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config
ADD PRIMARY KEY (merchant_operating_city_id);

ALTER TABLE atlas_driver_offer_bpp.merchant_message
ADD PRIMARY KEY (merchant_operating_city_id, message_key);

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
ADD PRIMARY KEY (merchant_operating_city_id, document_type);

ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD PRIMARY KEY (merchant_operating_city_id);


--ride
ALTER TABLE atlas_driver_offer_bpp.ride ADD CONSTRAINT fk_merchant_id FOREIGN KEY (merchant_id) REFERENCES atlas_driver_offer_bpp.merchant(id);
ALTER TABLE atlas_driver_offer_bpp.ride ADD CONSTRAINT fk_merchant_operating_city_id FOREIGN KEY (merchant_operating_city_id) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);