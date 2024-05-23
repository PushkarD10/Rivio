CREATE TABLE atlas_app.frfs_recon ();

ALTER TABLE atlas_app.frfs_recon ADD COLUMN beneficiary_ifsc text ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN buyer_finder_fee double precision NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN collector_ifsc text ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN collector_subscriber_id text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN date text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN destination_station_code text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN difference_amount double precision ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN fare double precision NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN frfs_ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN message text ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN mobile_number text ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN network_order_id text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN receiver_subscriber_id text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN settlement_amount double precision NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN settlement_date timestamp with time zone ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN settlement_reference_number text ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN source_station_code text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN ticket_number text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN ticket_qty integer NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN time text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN total_order_value double precision NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN transaction_ref_number text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_recon ADD PRIMARY KEY ( id);


------- SQL updates -------
ALTER TABLE atlas_app.frfs_recon ADD COLUMN beneficiary_bank_account text ;


------- SQL updates -------
ALTER TABLE atlas_app.frfs_recon ADD COLUMN transaction_uuid text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_recon ADD COLUMN currency text ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN txn_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_recon ADD COLUMN ticket_status text ;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN provider_name text NOT NULL;
ALTER TABLE atlas_app.frfs_recon ADD COLUMN provider_id text NOT NULL;