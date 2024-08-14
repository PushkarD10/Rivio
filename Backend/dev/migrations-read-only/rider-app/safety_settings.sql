CREATE TABLE atlas_app.safety_settings ();

ALTER TABLE atlas_app.safety_settings ADD COLUMN auto_call_default_contact boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN enable_post_ride_safety_check boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN enable_unexpected_events_check boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN false_safety_alarm_count integer ;
ALTER TABLE atlas_app.safety_settings ADD COLUMN has_completed_mock_safety_drill boolean  default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN has_completed_safety_setup boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN inform_police_sos boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN night_safety_checks boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN notify_safety_team_for_safety_check_failure boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN notify_sos_with_emergency_contacts boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.safety_settings ADD COLUMN safety_center_disabled_on_date timestamp with time zone ;
ALTER TABLE atlas_app.safety_settings ADD COLUMN shake_to_activate boolean NOT NULL default false;
ALTER TABLE atlas_app.safety_settings ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.safety_settings ADD PRIMARY KEY ( person_id);


------- SQL updates -------

ALTER TABLE atlas_app.safety_settings ADD COLUMN enable_otp_less_ride boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.safety_settings ALTER COLUMN enable_otp_less_ride SET DEFAULT false;
ALTER TABLE atlas_app.safety_settings ALTER COLUMN enable_otp_less_ride SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.safety_settings ADD COLUMN has_setup_ride_otp boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.safety_settings ALTER COLUMN has_setup_ride_otp SET NOT NULL;