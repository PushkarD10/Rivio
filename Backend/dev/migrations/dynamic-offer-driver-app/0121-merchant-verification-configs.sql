ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN verification_service character varying(30);
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET verification_service ='Idfy';
ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN verification_service SET NOT NULL;

-- { "accountId" : "xxxxxxx",
--   "apiKey" : "xxxxxxx",
--   "secret" : "xxxxxxx",
--   "url" : "http://localhost:6235"
-- }

WITH MerchantMapsServiceConfigs AS (
  SELECT T1.id, 'Verification_Idfy', CAST ('{
    "url":"http://localhost:6235",
    "accountId":"0.1.0|2|1svL1Ctvya1VY7ekcb9E2hFGi5D6lqcDlzWszP2YeRhKYKrEjBtQjz71/wsndEaYHuYOYE3rzLADQg==",
    "apiKey":"0.1.0|0|Udh/GePdRBOIlvu4b+srX9+UMY/SyesaszYDYEJAOCbDuA2J2/a5mANIKM7DmnKD+m6rma8jIADl8w==",
    "secret":"0.1.0|2|9MZiqPynVejras1kU4DUOWE9UqkOjBF12Z0zIPfGJTeWzgn5h8nMWjPv4ee/OpoMzVnEWhohJRZwzg=="
  }' AS json)
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantMapsServiceConfigs);
