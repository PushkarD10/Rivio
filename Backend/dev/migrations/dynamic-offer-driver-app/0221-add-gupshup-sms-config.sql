INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Sms_GupShup',
  json_build_object(
      'userName','0.1.0|2|z47JjbeG+bn+dOzrt3lfOtFe71jyw1rIiXI9ledgFzrSaky7M5quZXE5YpSs2hbfcyjC00BV4pia6cTUDw=='
    , 'password','0.1.0|2|z47JjbeG+bn+dOzrt3lfOtFe71jyw1rIiXI9ledgFzrSaky7M5quZXE5YpSs2hbfcyjC00BV4pia6cTUDw=='
    , 'url','enterprise.smsgupshup.com'
    , 'templateId','0.1.0|2|z47JjbeG+bn+dOzrt3lfOtFe71jyw1rIiXI9ledgFzrSaky7M5quZXE5YpSs2hbfcyjC00BV4pia6cTUDw=='
  )
FROM atlas_driver_offer_bpp.merchant m;

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET sms_providers_priority_list='{"MyValueFirst", "ExotelSms", "GupShup"}';