#!/bin/bash
SCHEMA=$1
ACTION=$2

if [ $ACTION = test_data_file ] ; then
  if [ $SCHEMA = atlas_app ] ; then
    echo "rider-app.sql"
  elif [ $SCHEMA = atlas_transporter ] ; then
    echo "static-offer-driver-app.sql"
  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
    echo "fmd-wrapper.sql"
  elif [ $SCHEMA = atlas_registry ] ; then
    echo "mock-registry.sql"
  elif [ $SCHEMA = atlas_public_transport ] ; then
    echo "public-transport-rider-platform.sql"
  fi
fi

if [ $ACTION = extra_test_data_tables ] ; then
  if [ $SCHEMA = atlas_app ] ; then
    echo 'location_backup search_request_location'
  elif [ $SCHEMA = atlas_transporter ] ; then
    echo 'driver_location fare_policy_discount location_backup fare_policy_per_extra_km_rate'
  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
    echo ''
  elif [ $SCHEMA = atlas_registry ] ; then
    echo ''
  elif [ $SCHEMA = atlas_public_transport ] ; then
    echo ''
  fi
fi

if [ $ACTION = seed_file ] ; then
  if [ $SCHEMA = atlas_app ] ; then
    echo 'rider-app-seed.sql'
  elif [ $SCHEMA = atlas_transporter ] ; then
    echo 'static-offer-driver-app-seed.sql'
  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
    echo 'fmd-wrapper-backend-seed.sql'
  elif [ $SCHEMA = atlas_registry ] ; then
    echo 'mock-registry-seed.sql'
  elif [ $SCHEMA = atlas_public_transport ] ; then
    echo 'public-transport-rider-platform-seed.sql'
  elif [ $SCHEMA = atlas_special_zone ] ; then
    echo 'special-zone-seed.sql'
  fi
fi

if [ $ACTION = migration_dir ] ; then
  if [ $SCHEMA = atlas_app ] ; then
    echo 'rider-app'
  elif [ $SCHEMA = atlas_transporter ] ; then
    echo 'static-offer-driver-app'
  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
    echo 'fmd-wrapper'
  elif [ $SCHEMA = atlas_registry ] ; then
    echo 'mock-registry'
  elif [ $SCHEMA = atlas_public_transport ] ; then
    echo 'public-transport-rider-platform'
  elif [ $SCHEMA = atlas_special_zone ] ; then
    echo "atlas-special-zone"
  fi
fi



#if [ $ACTION = test_data ] ; then
#  if [ $SCHEMA = atlas_app ] ; then
#    echo
#  elif [ $SCHEMA = atlas_transporter ] ; then
#    echo
#  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
#    echo
#  elif [ $SCHEMA = atlas_registry ] ; then
#    echo
#  elif [ $SCHEMA = atlas_public_transport ] ; then
#    echo
#  fi
#fi


