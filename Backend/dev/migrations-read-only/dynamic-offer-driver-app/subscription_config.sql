CREATE TABLE atlas_driver_offer_bpp.subscription_config ();

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN allow_driver_fee_calc_schedule boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN allow_due_addition boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN allow_manual_payment_links boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN deep_link_expiry_time_in_minutes integer ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN generic_batch_size_for_jobs integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN generic_job_reschedule_time integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN is_triggered_at_end_ride boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN max_retry_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN payment_link_channel text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN payment_link_job_time integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN payment_service_name text NOT NULL default 'Payment_Juspay';
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN send_deep_link boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN send_in_app_fcm_notifications boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN service_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN use_overlay_service boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD PRIMARY KEY ( service_name, merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN sgst_percentage_one_time_security_deposit double precision ;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ADD COLUMN cgst_percentage_one_time_security_deposit double precision ;