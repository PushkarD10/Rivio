{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.Accessor (
  module Services.Accessor,
  module Reexport
) where

import Engineering.Helpers.Accessor as Reexport

import Prelude
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)

_id :: forall a b c. Newtype a {id :: c | b} => Lens' a c
_id = lens (unwrap >>> _.id) (\oldRec newVal -> wrap ((unwrap oldRec) {id = newVal}))

_list :: forall a b c. Newtype a { list :: b | c} => Lens' a b
_list = lens (unwrap >>> _.list) (\oldRec newVal -> wrap ((unwrap oldRec) {list = newVal}))

_status :: forall a b c. Newtype a { status :: b | c} => Lens' a b
_status = lens (unwrap >>> _.status) (\oldRec newVal -> wrap ((unwrap oldRec) {status = newVal}))

_vehicleModel :: forall a b c. Newtype a { vehicleModel :: b | c} => Lens' a b
_vehicleModel = lens (unwrap >>> _.vehicleModel) (\oldRec newVal -> wrap ((unwrap oldRec) {vehicleModel = newVal}))

_createdAt :: forall a b c. Newtype a { createdAt :: b | c} => Lens' a b
_createdAt = lens (unwrap >>> _.createdAt) (\oldRec newVal -> wrap ((unwrap oldRec) {createdAt = newVal}))

_driverNumber :: forall a b c. Newtype a { driverNumber :: b | c} => Lens' a b
_driverNumber = lens (unwrap >>> _.driverNumber) (\oldRec newVal -> wrap ((unwrap oldRec) {driverNumber = newVal}))

_shortRideId :: forall a b c. Newtype a { shortRideId :: b | c} => Lens' a b
_shortRideId = lens (unwrap >>> _.shortRideId) (\oldRec newVal -> wrap ((unwrap oldRec) {shortRideId = newVal}))

_vehicleNumber :: forall a b c. Newtype a { vehicleNumber :: b | c} => Lens' a b
_vehicleNumber = lens (unwrap >>> _.vehicleNumber) (\oldRec newVal -> wrap ((unwrap oldRec) {vehicleNumber = newVal}))

_driverName :: forall a b c. Newtype a { driverName :: b | c} => Lens' a b
_driverName = lens (unwrap >>> _.driverName) (\oldRec newVal -> wrap ((unwrap oldRec) {driverName = newVal}))

_driverSelectedFare :: forall a b c. Newtype a { driverSelectedFare :: b | c} => Lens' a b
_driverSelectedFare = lens (unwrap >>> _.driverSelectedFare) (\oldRec newVal -> wrap ((unwrap oldRec) {driverSelectedFare = newVal}))

_actualRideDistance :: forall a b c. Newtype a { actualRideDistance :: b | c} => Lens' a b
_actualRideDistance = lens (unwrap >>> _.actualRideDistance) (\oldRec newVal -> wrap ((unwrap oldRec) {actualRideDistance = newVal}))

_vehicleVariant :: forall a b c. Newtype a { vehicleVariant :: b | c} => Lens' a b
_vehicleVariant = lens (unwrap >>> _.vehicleVariant) (\oldRec newVal -> wrap ((unwrap oldRec) {vehicleVariant = newVal}))

_estimatedBaseFare :: forall a b c. Newtype a { estimatedBaseFare :: b | c} => Lens' a b
_estimatedBaseFare = lens (unwrap >>> _.estimatedBaseFare) (\oldRec newVal -> wrap ((unwrap oldRec) {estimatedBaseFare = newVal}))

_vehicleColor :: forall a b c. Newtype a { vehicleColor :: b | c} => Lens' a b
_vehicleColor = lens (unwrap >>> _.vehicleColor) (\oldRec newVal -> wrap ((unwrap oldRec) {vehicleColor = newVal}))

_updatedAt :: forall a b c. Newtype a { updatedAt :: b | c} => Lens' a b
_updatedAt = lens (unwrap >>> _.updatedAt) (\oldRec newVal -> wrap ((unwrap oldRec) {updatedAt = newVal}))

_lat :: forall a b c. Newtype a { lat :: b | c} => Lens' a b
_lat = lens (unwrap >>> _.lat) (\oldRec newVal -> wrap ((unwrap oldRec) {lat = newVal}))


_lon :: forall a b c. Newtype a { lon :: b | c} => Lens' a b
_lon = lens (unwrap >>> _.lon) (\oldRec newVal -> wrap ((unwrap oldRec) {lon = newVal}))

_certificateNumber :: forall a b c. Newtype a { certificateNumber :: b | c} => Lens' a b 
_certificateNumber = lens (unwrap >>> _.certificateNumber) (\oldRec newVal -> wrap (unwrap oldRec) {certificateNumber = newVal})

_distance :: forall a b c. Newtype a {distance :: c | b} => Lens' a c
_distance = lens (unwrap >>> _.distance) (\oldRec newVal -> wrap ((unwrap oldRec) {distance = newVal}))

_orderId :: forall a b c. Newtype a { orderId :: b | c } => Lens' a b
_orderId = lens (unwrap >>> _.orderId) (\oldRec newVal -> wrap ((unwrap oldRec) { orderId = newVal }))

_moduleId :: forall a b c. Newtype a { moduleId :: b | c } => Lens' a b
_moduleId = lens (unwrap >>> _.moduleId) (\oldRec newVal -> wrap ((unwrap oldRec) { moduleId = newVal }))

_name :: forall a b c. Newtype a { name :: b | c } => Lens' a b
_name = lens (unwrap >>> _.name) (\oldRec newVal -> wrap ((unwrap oldRec) { name = newVal }))

_languagesAvailableForQuiz :: forall a b c. Newtype a { languagesAvailableForQuiz :: b | c } => Lens' a b
_languagesAvailableForQuiz = lens (unwrap >>> _.languagesAvailableForQuiz) (\oldRec newVal -> wrap ((unwrap oldRec) { languagesAvailableForQuiz = newVal }))

_languagesAvailableForVideos :: forall a b c. Newtype a { languagesAvailableForVideos :: b | c } => Lens' a b
_languagesAvailableForVideos = lens (unwrap >>> _.languagesAvailableForVideos) (\oldRec newVal -> wrap ((unwrap oldRec) { languagesAvailableForVideos = newVal }))

_area :: forall a b c. Newtype a { area :: b | c } => Lens' a b
_area = lens (unwrap >>> _.area) (\oldRec newVal -> wrap ((unwrap oldRec) { area = newVal }))

_extras :: forall a b c. Newtype a { extras :: b | c } => Lens' a b
_extras = lens (unwrap >>> _.extras) (\oldRec newVal -> wrap ((unwrap oldRec) { extras = newVal }))

_instructions :: forall a b c. Newtype a { instructions :: b | c } => Lens' a b
_instructions = lens (unwrap >>> _.instructions) (\oldRec newVal -> wrap ((unwrap oldRec) { instructions = newVal }))


_routeCode :: forall a b c. Newtype a { code :: b | c } => Lens' a b
_routeCode = lens (unwrap >>> _.code) (\oldRec newVal -> wrap ((unwrap oldRec) { code = newVal }))

_routeShortName :: forall a b c. Newtype a { shortName :: b | c } => Lens' a b
_routeShortName = lens (unwrap >>> _.shortName) (\oldRec newVal -> wrap ((unwrap oldRec) { shortName = newVal }))

_routeLongName :: forall a b c. Newtype a { longName :: b | c } => Lens' a b
_routeLongName = lens (unwrap >>> _.longName) (\oldRec newVal -> wrap ((unwrap oldRec) { longName = newVal }))

_routeStartPoint :: forall a b c. Newtype a { startPoint :: b | c } => Lens' a b
_routeStartPoint = lens (unwrap >>> _.startPoint) (\oldRec newVal -> wrap ((unwrap oldRec) { startPoint = newVal }))

_routeEndPoint :: forall a b c. Newtype a { endPoint :: b | c } => Lens' a b
_routeEndPoint = lens (unwrap >>> _.endPoint) (\oldRec newVal -> wrap ((unwrap oldRec) { endPoint = newVal }))

_busNumber :: forall a b c. Newtype a { number :: b | c } => Lens' a b
_busNumber = lens (unwrap >>> _.number) (\oldRec newVal -> wrap ((unwrap oldRec) { number = newVal }))

_busType :: forall a b c. Newtype a { _type :: b | c } => Lens' a b
_busType = lens (unwrap >>> _._type) (\oldRec newVal -> wrap ((unwrap oldRec) { _type = newVal }))

_stopName :: forall a b c. Newtype a { name :: b | c } => Lens' a b
_stopName = lens (unwrap >>> _.name) (\oldRec newVal -> wrap ((unwrap oldRec) { name = newVal }))

_stopCode :: forall a b c. Newtype a { code :: b | c } => Lens' a b
_stopCode = lens (unwrap >>> _.code) (\oldRec newVal -> wrap ((unwrap oldRec) { code = newVal }))

_stopLat :: forall a b c. Newtype a { lat :: b | c } => Lens' a b
_stopLat = lens (unwrap >>> _.lat) (\oldRec newVal -> wrap ((unwrap oldRec) { lat = newVal }))

_stopLong :: forall a b c. Newtype a { long :: b | c } => Lens' a b
_stopLong = lens (unwrap >>> _.long) (\oldRec newVal -> wrap ((unwrap oldRec) { long = newVal }))

_routeInfo :: forall a b c. Newtype a { routeInfo :: b | c } => Lens' a b
_routeInfo = lens (unwrap >>> _.routeInfo) (\oldRec newVal -> wrap ((unwrap oldRec) { routeInfo = newVal }))

_source :: forall a b c. Newtype a { source :: b | c } => Lens' a b
_source = lens (unwrap >>> _.source) (\oldRec newVal -> wrap ((unwrap oldRec) { source = newVal }))

_destination :: forall a b c. Newtype a { destination :: b | c } => Lens' a b
_destination = lens (unwrap >>> _.destination) (\oldRec newVal -> wrap ((unwrap oldRec) { destination = newVal }))

_vehicleDetails :: forall a b c. Newtype a { vehicleDetails :: b | c } => Lens' a b
_vehicleDetails = lens (unwrap >>> _.vehicleDetails) (\oldRec newVal -> wrap ((unwrap oldRec) { vehicleDetails = newVal }))

_allowStartRideFromQR :: forall a b c. Newtype a { allowStartRideFromQR :: b | c } => Lens' a b
_allowStartRideFromQR = lens (unwrap >>> _.allowStartRideFromQR) (\oldRec newVal -> wrap ((unwrap oldRec) { allowStartRideFromQR = newVal }))