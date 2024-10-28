{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.BusTrackingScreen.Transformer where


import Prelude 
import Screens.Types 
import Services.API as API
import Data.Array as DA
import Data.Maybe
import Engineering.Helpers.Commons
import Data.Function.Uncurried as DFU
import Helpers.Utils as HU
import JBridge as JB
import Services.Backend as Remote 
import Constants.Configs (getPolylineAnimationConfig)
import Debug

transformStationsForMap :: Array API.FRFSStationAPI -> API.Route -> String -> String -> JB.RouteConfig
transformStationsForMap stations (API.Route route) srcCode destCode = do
  let res = DA.foldl (\acc word -> if acc.index == 0 then acc{src = Just word, index = acc.index + 1}
                      else if acc.index == (DA.length stations -1)  then acc{dest = Just word, index = acc.index + 1}
                      else acc{stops = acc.stops <> [word], index = acc.index + 1} ) {src : Nothing, dest : Nothing, stops : [], index : 0} stations
      _  = spy "res" res
  case res.src, res.dest of
    Just (API.FRFSStationAPI s), Just (API.FRFSStationAPI d) -> do
  -- let 
  -- mbSource = maybe Nothing (\(API.FRFSStationAPI item) -> Just item) (res.src) 
  --     mbDest = maybe Nothing (\(API.FRFSStationAPI item) -> Just item) (res.dest)
      let stops = res.stops
          _ = spy "s" s
          _ = spy "sstops" stops
          _ = spy "d" d
          markers = HU.normalRoute ""
          len = DA.length stops
          sourcePosition = {lat : fromMaybe 0.0 s.lat, lng :fromMaybe 0.0  s.lon}
          destPosition = {lat : fromMaybe 0.0 d.lat, lng :fromMaybe 0.0 d.lon}
          srcMarkerConfig = JB.defaultMarkerConfig{ markerId = s.code, pointerIcon = getMarkerImage s.code 0 len, primaryText = s.name, position = sourcePosition, markerSize = if getMarkerImage s.code 0 len == "ny_ic_stop_black" then 20.0 else 90.0, useMarkerSize = true}
          destMarkerConfig = JB.defaultMarkerConfig{ markerId = d.code, pointerIcon = getMarkerImage d.code (len-1) len, primaryText = d.name, position = destPosition, markerSize = if getMarkerImage d.code (len-1) len == "ny_ic_stop_black" then 20.0 else 90.0, useMarkerSize = true}
          stopsConfig = DA.mapWithIndex (\index (API.FRFSStationAPI item) -> JB.defaultMarkerConfig{ markerId = item.code, pointerIcon = getMarkerImage item.code (index+1) len, position = {lat : fromMaybe 0.0 item.lat, lng : fromMaybe 0.0  item.lon}, markerSize = 10.0, useMarkerSize = true}) stops
      JB.mkRouteConfig (Remote.walkCoordinates route.points) srcMarkerConfig destMarkerConfig (Just stopsConfig) "NORMAL" "LineString" true JB.DEFAULT $ HU.mkMapRouteConfig "" "" false getPolylineAnimationConfig 
    _,_ -> JB.routeConfig
    where 
      getMarkerImage code index ln = if srcCode /= "" && srcCode == code then markers.srcMarker 
                            else if destCode /= "" && destCode == code then markers.destMarker
                            else if srcCode /= "" && destCode /= "" then "ny_ic_stop_black"
                            else if index == 0 then markers.srcMarker 
                            else if index == ln  then markers.destMarker
                            else "ny_ic_stop_black"
      markers = HU.normalRoute ""

  -- let mbSrcIndex = DA.findIndex (\(API.FRFSStationAPI item) -> item.code == srcCode) stations
  --     mbDestIndex = DA.findIndex (\(API.FRFSStationAPI item) -> item.code == destCode) stations
  -- case mbSrcIndex, mbDestIndex of
  --   Just srcIndex, Just destIndex -> do
  --     let mbSource = maybe Nothing (\(API.FRFSStationAPI item) -> Just item) (stations DA.!! srcIndex) 
  --         mbDest = maybe Nothing (\(API.FRFSStationAPI item) -> Just item) (stations DA.!! destIndex)
  --         stops = DA.slice (srcIndex + 1) destIndex stations
  --         markers = HU.normalRoute ""
  --         sourcePosition = case mbSource of
  --                            Just s -> {lat : fromMaybe 0.0 s.lat, lng :fromMaybe 0.0  s.lon}
  --                            _ -> {lat : 0.0, lng : 0.0}
  --         destPosition = case mbDest of
  --                            Just s -> {lat : fromMaybe 0.0 s.lat, lng :fromMaybe 0.0 s.lon}
  --                            _ -> {lat : 0.0, lng : 0.0}
  --         srcMarkerConfig = JB.defaultMarkerConfig{ markerId = "src", pointerIcon = markers.srcMarker, primaryText = fromMaybe "Source" (mbSource <#> _.name), position = sourcePosition}
  --         destMarkerConfig = JB.defaultMarkerConfig{ markerId = "dest", pointerIcon = markers.destMarker, primaryText = fromMaybe "Dest" (mbDest <#> _.name), position = destPosition}
  --         stopsConfig = map (\(API.FRFSStationAPI item) -> JB.defaultMarkerConfig{ markerId = item.code, pointerIcon = markers.srcMarker, position = {lat : fromMaybe 0.0 item.lat, lng : fromMaybe 0.0  item.lon}}) stops
  --     JB.mkRouteConfig (Remote.walkCoordinates route.points) srcMarkerConfig destMarkerConfig (Just stopsConfig) "NORMAL" "LineString" true JB.DEFAULT $ HU.mkMapRouteConfig "" "" false getPolylineAnimationConfig
  --   _,_ -> JB.routeConfig
  -- where
  --   getPosition stop =  {
  --     lat : fromMaybe 0.0 (stop <#> _.lat)
  --     lng : fromMaybe 0.0 (stop <#> _.lon)
  --   }

getStationsFromBusRoute ::  API.FRFSRouteAPI -> Array API.FRFSStationAPI 
getStationsFromBusRoute (API.FRFSRouteAPI stop) = fromMaybe [] stop.stations
