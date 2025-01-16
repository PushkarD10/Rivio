INSERT INTO atlas_bpp_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bpp_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'DRIVERS' as api_entity,
        'REFUND_BY_PAYOUT' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bpp_dashboard.role)
ON CONFLICT DO NOTHING;


INSERT INTO atlas_bpp_dashboard.access_matrix
        (id, role_id, api_entity, user_action_type, user_access_type, created_at, updated_at)
(SELECT atlas_bpp_dashboard.uuid_generate_v4() as id,
        id as role_id,
        'DRIVERS' as api_entity,
        'SECURITY_DEPOSIT_STATUS' as user_action_type,
        'USER_FULL_ACCESS' as user_access_type,
        now() as created_at,
        now() as updated_at from atlas_bpp_dashboard.role)
ON CONFLICT DO NOTHING;
