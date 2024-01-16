-- ALTER QUERIES
insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('125378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 50);
insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('225378b5-0a86-44ce-afa1-b4d27d712a23', 'Kid', 'Kid', 20);

insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('325378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 20);
insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('425378b5-0a86-44ce-afa1-b4d27d712a23', 'Kid', 'Kid', 10);

insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('525378b5-0a86-44ce-afa1-b4d27d712a23', 'CameraUnit', 'CameraUnit', 250);

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c15378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{125378b5-0a86-44ce-afa1-b4d27d712a23, 225378b5-0a86-44ce-afa1-b4d27d712a23}');
insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c25378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{325378b5-0a86-44ce-afa1-b4d27d712a23, 425378b5-0a86-44ce-afa1-b4d27d712a23}');
insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c35378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{525378b5-0a86-44ce-afa1-b4d27d712a23}');

insert into atlas_app.business_hour (id, btype, category_id) values ('b15378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 03:30:00 11:30:00', '{c15378b5-0a86-44ce-afa1-b4d27d712a23}');
insert into atlas_app.business_hour (id, btype, category_id) values ('b25378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 05:00:00 11:00:00', '{c25378b5-0a86-44ce-afa1-b4d27d712a23}');
insert into atlas_app.business_hour (id, btype, category_id) values ('b35378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 03:30:00 11:30:00', '{c35378b5-0a86-44ce-afa1-b4d27d712a23}');

update atlas_app.ticket_service set allow_future_booking = true, expiry = 'VisitDate 11:00:00';

update atlas_app.ticket_service set business_hours = '{b15378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'b73378dc-427f-4efa-9b55-8efe7e3352c2';
update atlas_app.ticket_service set business_hours = '{b25378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'a7eba6ed-99f7-442f-a9d8-00c8b380657b';
update atlas_app.ticket_service set business_hours = '{b35378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'd8f47b42-50a5-4a97-8dda-e80a3633d7ab';

-- data for jetty booking
-- ticket place
insert into atlas_app.ticket_place (id, merchant_operating_city_id, name, description, lat, lon, gallery, open_timings, close_timings, short_desc, icon_url, map_image_url, terms_and_conditions, place_type) values ('a1dbacd1-6128-7a0f-226f-9129c96d0101', 'da7e19c8-2d86-66ea-039c-edfd8e34ebdc', 'Millenium Park Shipping Jetty', 'Millenium Park Ferry is a famous jetty in Kolkata and a big tourist attraction in Kolkata, West Bengal', 22.57544778324872, 88.34578097649805, '{https://firebasestorage.googleapis.com/v0/b/jp-beckn-dev.appspot.com/o/zoo%2Fny_ic_ferry_bg.png?alt=media&token=55707903-4411-4122-8dbc-9ce9176c8b7a}', '02:30:00', '14:30:00', 'Book Ferry Tickets across the Hoogli', 'https://firebasestorage.googleapis.com/v0/b/jp-beckn-dev.appspot.com/o/zoo%2Fny_ic_ferry_bg.png?alt=media&token=55707903-4411-4122-8dbc-9ce9176c8b7a', 'https://firebasestorage.googleapis.com/v0/b/jp-beckn-dev.appspot.com/o/zoo%2Fny_ic_ferry_location.png?alt=media&token=434e3dcd-4efc-436a-9671-20e67aef4e37', '{"Cancellation of tickets is not applicable","Tickets are valid only for an hour from booking time"}', 'WaterPark');

-- people category

insert into atlas_app.service_people_category (description, id, name, price_per_unit) values ('Passenger Vessel', 'ebfef970-5e49-0e40-9529-222354e1783a', 'Passenger Vessel', 6);

insert into atlas_app.service_people_category (description, id, name, price_per_unit) values ('Passenger Vessel', 'ebfef970-5e49-0e40-9529-222354e1783b', 'Passenger Vessel', 6);

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', '7b03af02-bf06-2c52-3aa6-5042f50b2153', 'all', '{ebfef970-5e49-0e40-9529-222354e1783a}');

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', '7b03af02-bf06-2c52-3aa6-5042f50b2154', 'all', '{ebfef970-5e49-0e40-9529-222354e1783b}');


-- normal
insert into atlas_app.business_hour (btype, category_id, id) values ('Duration 02:30:00 14:30:00', '{7b03af02-bf06-2c52-3aa6-5042f50b2153}', '985be4e6-dd20-f0d4-0f34-63b655d84327');
insert into atlas_app.business_hour (btype, category_id, id) values ('Duration 02:30:00 13:50:00', '{7b03af02-bf06-2c52-3aa6-5042f50b2153}', '885be4e6-dd20-f0d4-0f34-63b655d84327');

insert into atlas_app.business_hour (btype, category_id, id) values ('Duration 02:30:00 14:30:00', '{7b03af02-bf06-2c52-3aa6-5042f50b2153}', '585be4e6-dd20-f0d4-0f34-63b655d84327');
insert into atlas_app.business_hour (btype, category_id, id) values ('Duration 02:30:00 13:50:00', '{7b03af02-bf06-2c52-3aa6-5042f50b2153}', '485be4e6-dd20-f0d4-0f34-63b655d84327');

insert into atlas_app.business_hour (btype, category_id, id) values ('Duration 02:30:00 14:30:00', '{7b03af02-bf06-2c52-3aa6-5042f50b2154}', '785be4e6-dd20-f0d4-0f34-63b655d84327');
insert into atlas_app.business_hour (btype, category_id, id) values ('Duration 02:30:00 13:50:00', '{7b03af02-bf06-2c52-3aa6-5042f50b2154}', '685be4e6-dd20-f0d4-0f34-63b655d84327');

-- ticket service

insert into atlas_app.ticket_service(id, places_id, service, max_verification, allow_future_booking, business_hours, expiry, operational_days, short_desc) values ('efaee967-8598-0a73-6b90-bee9b7f3f42b', 'a1dbacd1-6128-7a0f-226f-9129c96d0101', 'Passenger Ferry (to Howrah)', 1, false, '{985be4e6-dd20-f0d4-0f34-63b655d84327}', 'InstantExpiry 60', '{Monday,Tuesday,Wednesday,Thursday,Friday}', 'Shipping Jetty');

insert into atlas_app.ticket_service(id, places_id, service, max_verification, allow_future_booking, business_hours, expiry, operational_days, short_desc) values ('ffaee967-8598-0a73-6b90-bee9b7f3f42b', 'a1dbacd1-6128-7a0f-226f-9129c96d0101', 'Passenger Ferry (to Millennium)', 1, false, '{785be4e6-dd20-f0d4-0f34-63b655d84327}', 'InstantExpiry 60', '{Monday,Tuesday,Wednesday,Thursday,Friday}', 'Shipping Jetty');

insert into atlas_app.special_occasion (business_hours, date, day_of_week, description, entity_id, id, special_day_type) values ('{885be4e6-dd20-f0d4-0f34-63b655d84327}', null, 'Saturday', 'Early closing', 'efaee967-8598-0a73-6b90-bee9b7f3f42b', atlas_app.uuid_generate_v4(), 'Open');

insert into atlas_app.special_occasion (business_hours, date, day_of_week, description, entity_id, id, special_day_type) values ('{685be4e6-dd20-f0d4-0f34-63b655d84327}', null, 'Saturday', 'Early closing', 'ffaee967-8598-0a73-6b90-bee9b7f3f42b', atlas_app.uuid_generate_v4(), 'Open');

insert into atlas_app.special_occasion (business_hours, date, day_of_week, description, entity_id, id, special_day_type) values ('{585be4e6-dd20-f0d4-0f34-63b655d84327}', null, 'Sunday', 'Holiday', 'efaee967-8598-0a73-6b90-bee9b7f3f42b', atlas_app.uuid_generate_v4(), 'Closed');

insert into atlas_app.special_occasion (business_hours, date, day_of_week, description, entity_id, id, special_day_type) values ('{485be4e6-dd20-f0d4-0f34-63b655d84327}', null, 'Sunday', 'Holiday', 'ffaee967-8598-0a73-6b90-bee9b7f3f42b', atlas_app.uuid_generate_v4(), 'Closed');

-- data for cruise
insert into atlas_app.ticket_place (id, merchant_operating_city_id, name, description, lat, lon, gallery, open_timings, close_timings, short_desc, icon_url, map_image_url, terms_and_conditions, place_type) values ('b1dbacd1-6128-7a0f-226f-9129c96d0101', 'da7e19c8-2d86-66ea-039c-edfd8e34ebdc', 'Kolkata Heritage River Cruise', 'The Kolkata Heritage River Cruise is a one-hour joy ride featuring live music. It operates once daily on weekdays and twice on weekends. The cruise begins from the Shipping Jetty and returns to the same place.', 22.572468299943697, 88.34414214676373, '{https://firebasestorage.googleapis.com/v0/b/jp-beckn-dev.appspot.com/o/zoo%2Fny_ic_cruise_bg.png?alt=media&token=27f92602-96e8-4f57-9684-7b9766eee1dc}', '02:30:00', '14:30:00', 'Book Joy rides on the Hoogli', 'https://firebasestorage.googleapis.com/v0/b/jp-beckn-dev.appspot.com/o/zoo%2Fny_ic_cruise_bg.png?alt=media&token=27f92602-96e8-4f57-9684-7b9766eee1dc', 'https://firebasestorage.googleapis.com/v0/b/jp-beckn-dev.appspot.com/o/zoo%2Fny_ic_cruise_location.png?alt=media&token=522ffb12-b836-418a-a9a5-5b9fb787a214', '{"Cancellation of tickets is not applicable"}', 'WaterPark');

-- people category
insert into atlas_app.service_people_category (description, id, name, price_per_unit) values ('Cruise', 'cbfef970-5e49-0e40-9529-222354e1783a', 'Cruise', 169);

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (150, 150, 'all', '8b03af02-bf06-2c52-3aa6-5042f50b2153', 'all', '{cbfef970-5e49-0e40-9529-222354e1783a}');

-- normal
insert into atlas_app.business_hour (btype, category_id, id) values ('Slot 12:30:00', '{8b03af02-bf06-2c52-3aa6-5042f50b2153}', '185be4e6-dd20-f0d4-0f34-63b655d84327');

insert into atlas_app.business_hour (btype, category_id, id) values ('Slot 11:30:00', '{8b03af02-bf06-2c52-3aa6-5042f50b2153}', '285be4e6-dd20-f0d4-0f34-63b655d84327');

insert into atlas_app.business_hour (btype, category_id, id) values ('Slot 13:15:00', '{8b03af02-bf06-2c52-3aa6-5042f50b2153}', '385be4e6-dd20-f0d4-0f34-63b655d84327');

-- ticket service
insert into atlas_app.ticket_service(id, places_id, service, max_verification, allow_future_booking, business_hours, expiry, operational_days, short_desc) values ('afaee967-8598-0a73-6b90-bee9b7f3f42b', 'b1dbacd1-6128-7a0f-226f-9129c96d0101', 'Heritage Cruise', 1, true, '{185be4e6-dd20-f0d4-0f34-63b655d84327}', 'VisitDate 01:30:00', '{Monday,Tuesday,Wednesday,Thursday,Friday}', null);

insert into atlas_app.special_occasion (business_hours, date, day_of_week, description, entity_id, id, special_day_type) values ('{285be4e6-dd20-f0d4-0f34-63b655d84327, 385be4e6-dd20-f0d4-0f34-63b655d84327}', null, 'Saturday', 'Two slots', 'afaee967-8598-0a73-6b90-bee9b7f3f42b', atlas_app.uuid_generate_v4(), 'Open');

insert into atlas_app.special_occasion (business_hours, date, day_of_week, description, entity_id, id, special_day_type) values ('{285be4e6-dd20-f0d4-0f34-63b655d84327, 385be4e6-dd20-f0d4-0f34-63b655d84327}', null, 'Sunday', 'Two slots', 'afaee967-8598-0a73-6b90-bee9b7f3f42b', atlas_app.uuid_generate_v4(), 'Open');

-- DON'T RUN
-- insert into atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) (select atlas_bap_dashboard.uuid_generate_v4() as id,id as role_id,'CUSTOMERS' as api_entity, 'USER_NO_ACCESS' as user_access_type, now() as created_at, now() as updated_at, 'VERIFY_BOOKING_DETAILS' as user_action_type from atlas_bap_dashboard.role) on conflict do nothing;

-- insert into atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) (select atlas_bap_dashboard.uuid_generate_v4() as id,id as role_id,'CUSTOMERS' as api_entity, 'USER_NO_ACCESS' as user_access_type, now() as created_at, now() as updated_at, 'GET_TICKET_SERVICES' as user_action_type from atlas_bap_dashboard.role) on conflict do nothing;
