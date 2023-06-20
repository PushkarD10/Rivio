/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.widget.Toast;

import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

import com.google.firebase.analytics.FirebaseAnalytics;
import org.json.JSONException;
import org.json.JSONObject;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;

public class RideRequestUtils {
    private final int rideReqNotificationId = 5032023;
    private final String RIDE_REQUEST_CHANNEL = "in.juspay.mobility.riderequest";
    private final int rideReqNotificationReqCode = 6032023;
    private FirebaseAnalytics mFirebaseAnalytics;

    public Boolean driverRespondApi(String searchRequestId, int offeredPrice, boolean isAccept, Context context, int slotNumber) {
        Handler mainLooper = new Handler(Looper.getMainLooper());
        StringBuilder result = new StringBuilder();
        SharedPreferences sharedPref = context.getApplicationContext().getSharedPreferences(context.getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String bundle_version = sharedPref.getString("BUNDLE_VERSION", "null");
        String version = sharedPref.getString("VERSION_NAME", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS","null");
        try {
            String orderUrl = sharedPref.getString("BASE_URL", "null") + "/driver/searchRequest/quote/respond";
            HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("x-client-version", version);
            connection.setRequestProperty("token", sharedPref.getString(context.getResources().getString(R.string.REGISTERATION_TOKEN), "null"));
            connection.setRequestProperty("x-bundle-version", bundle_version);
            connection.setRequestProperty("x-device", deviceDetails);
            connection.setDoOutput(true);
            connection.setConnectTimeout(20000);
            connection.setReadTimeout(20000);
            JSONObject payload = new JSONObject();
            if (!isAccept || offeredPrice == 0) {
                payload.put(context.getResources().getString(R.string.OFFERED_FARE), null);
            } else {
                payload.put(context.getResources().getString(R.string.OFFERED_FARE), (offeredPrice));
            }
            payload.put(context.getResources().getString(R.string.SEARCH_REQUEST_ID), searchRequestId);
            if (isAccept) payload.put("response", "Accept");
            else payload.put("response", "Reject");
            payload.put("slotNumber", slotNumber);
            OutputStream stream = connection.getOutputStream();
            stream.write(payload.toString().getBytes());
            connection.connect();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                JSONObject errorPayload = new JSONObject(result.toString());
                if (errorPayload.has(context.getResources().getString(R.string.ERROR_MESSAGE))){
                    mainLooper.post(new Runnable() {
                        @Override
                        public void run() {
                            try {
                                Toast.makeText(context.getApplicationContext(), errorPayload.getString(context.getResources().getString(R.string.ERROR_MESSAGE)) , Toast.LENGTH_SHORT).show();
                            } catch (JSONException e) {
                                e.printStackTrace();
                            }
                        }
                    });
                }
            } else {
                //API Success
                return  true;
            }
            return false;
        } catch (SocketTimeoutException e){
            mainLooper.post(new Runnable() {
                @Override
                public void run() {
                    Toast.makeText(context.getApplicationContext(), "Request Timeout" , Toast.LENGTH_SHORT).show();
                }
            });
            return false;
        } catch (Exception e) {
            return false;
        }
    }


    public int calculateExpireTimer(String expireTimeTemp, String currTimeTemp){
        if (expireTimeTemp == null || currTimeTemp == null) return 0;
        String[] arrOfA = expireTimeTemp.split("T");
        String[] arrOfB = currTimeTemp.split("T");
        if(!arrOfA[0].equals(arrOfB[0])) return -1;

        String[] timeTempExpire = arrOfA[1].split(":");
        String[] timeTempCurrent = arrOfB[1].split(":");
        timeTempExpire[2] = timeTempExpire[2].substring(0,2);
        timeTempCurrent[2] = timeTempCurrent[2].substring(0,2);
        int currTime = 0, expireTime = 0, calculate = 3600;
        for(int i = 0 ; i < timeTempCurrent.length;i++){
            currTime+= (Integer.parseInt(timeTempCurrent[i])*calculate);
            expireTime+= (Integer.parseInt(timeTempExpire[i])*calculate);
            calculate = calculate/60;
        }
        if ((expireTime-currTime) >= 2) return expireTime-currTime - 2 ;
        return 0;
    }

    public void createRideRequestNotification(Context context){
        long[] vibrationPattern = {1000, 1000, 1000, 800, 800, 800, 800, 800, 800, 800, 800, 800};
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(RIDE_REQUEST_CHANNEL, "RideRequestChannel", NotificationManager.IMPORTANCE_HIGH);
            channel.enableVibration(true);
            channel.setVibrationPattern(vibrationPattern);
            channel.setLockscreenVisibility(Notification.VISIBILITY_PUBLIC);
            channel.setImportance(NotificationManager.IMPORTANCE_HIGH);
            NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
        Intent notificationIntent;
        if (RideRequestActivity.getInstance() == null){
            notificationIntent = new Intent(context, MainActivity.class);
        }else {
            notificationIntent = new Intent(context, RideRequestActivity.class);
        }
        notificationIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
        PendingIntent pendingIntent = PendingIntent.getActivity(context, rideReqNotificationReqCode, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context,RIDE_REQUEST_CHANNEL) ;
        mBuilder.setLargeIcon(BitmapFactory.decodeResource(context.getResources(), R.mipmap.ic_launcher));
        mBuilder.setContentTitle(context.getString(R.string.new_ride_req))
                .setContentText(context.getString(R.string.new_ride_available_for_offering))
                .setSmallIcon((R.mipmap.ic_launcher))
                .setAutoCancel(true)
                .setVibrate(vibrationPattern)
                .setSound(null)
                .setPriority(NotificationCompat.PRIORITY_HIGH);
        mBuilder.setContentIntent(pendingIntent);
        NotificationManagerCompat managerCompat = NotificationManagerCompat.from(context.getApplicationContext());
        managerCompat.notify(rideReqNotificationId, mBuilder.build());
    }


    public void cancelRideReqNotification(Context context) {
        String ns = Context.NOTIFICATION_SERVICE;
        NotificationManager notificationManager = (NotificationManager) context.getSystemService(ns);
        notificationManager.cancel(rideReqNotificationId);
    }

    public void firebaseLogEventWithParams(String event,String paramKey,String paramValue, Context context) {
        Bundle params = new Bundle();
        params.putString(paramKey,paramValue);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        mFirebaseAnalytics.logEvent(event, params);
    }

    public void restartLocationService(Context context)
    {
        Intent locationService = new Intent(context, LocationUpdateService.class);
        locationService.putExtra("StartingSource","TRIGGER_SERVICE");
        locationService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startForegroundService(locationService);
        }else{
            context.startService(locationService);
        }
    }

    public void callAPIViaFCM(String orderUrl, JSONObject requestBody, String method, Context context){
        SharedPreferences sharedPref = context.getSharedPreferences(
                context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
            StringBuilder result = new StringBuilder();
            try {
                System.out.println("in callAPIViaFCM");
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod(method);
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("token", token);
                connection.setRequestProperty("x-device", deviceDetails);
                connection.setDoOutput(true);

                OutputStream stream = connection.getOutputStream();
                if(requestBody!=null){
                    stream.write(requestBody.toString().getBytes());
                }
                connection.connect();
                int respCode = connection.getResponseCode();
                InputStreamReader respReader;

                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    respReader = new InputStreamReader(connection.getErrorStream());
                    firebaseLogEventWithParams("ny_fcm_error_calling_api","status_code", String.valueOf(respCode), context);
                    System.out.println("in error : " + respReader);
                } else {
                    respReader = new InputStreamReader(connection.getInputStream());
                    firebaseLogEventWithParams("ny_fcm_success_calling_api","status_code", String.valueOf(respCode), context);
                    System.out.println("in 200 : " + respReader);
                }

                BufferedReader in = new BufferedReader(respReader);
                String inputLine;

                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                System.out.println("in result : " + result.toString());

            } catch (Exception e) {
                System.out.println("Catch in callAPIViaFCM : " +e);
            }
            handler.post(executor::shutdown);
        });
    }

    public void openApplication(Context context) {
        Intent intent = new Intent(context.getApplicationContext(), MainActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
        try {
            context.getApplicationContext().startActivity(intent);
        } catch (Exception ignored) {}
    }


    public JSONObject getZoneConfig(String tag, Context context){
        try {
            String key = "";
            String[] arrOfStr = tag.split("_");
            String pickup = arrOfStr[0];
            String drop = arrOfStr[1];
            String priority = arrOfStr[2];
            if (priority.equals("PriorityPickup")) {
                key = pickup + "_Pickup";
            } else if (priority.equals("PriorityDrop")){
                key = drop + "_Drop";
            } else {
                return new JSONObject();
            }
            InputStream is = context.getAssets().open("juspay/zone_config.json");
            int size = is.available();
            byte[] buffer = new byte[size];
            is.read(buffer);
            is.close();
            String json = new String(buffer, StandardCharsets.UTF_8);
            return new JSONObject(json).getJSONObject(key);
        } catch (Exception ex) {
            ex.printStackTrace();
            return new JSONObject();
        }
    }

    public void setSpecialZoneAttrs(SheetAdapter.SheetViewHolder holder, String specialLocationTag, Context context) {
        try{
            JSONObject zoneConfig = getZoneConfig(specialLocationTag,context);
            holder.specialLocationTag.setCardBackgroundColor(Color.parseColor(zoneConfig.get("backgroundColor").toString()));
            holder.zoneDot.setCardBackgroundColor(Color.parseColor(zoneConfig.get("textColor").toString()));
            holder.zoneText.setText(zoneConfig.get("text").toString());
            holder.zoneText.setTextColor(Color.parseColor(zoneConfig.get("textColor").toString()));
            holder.fixedFareText.setText("No Cancellation");
            holder.fixedFareText.setTextColor(Color.parseColor(zoneConfig.get("textColor").toString()));
            holder.specialLocationTag.setVisibility(View.VISIBLE);
            holder.assetZonePickup.setImageURI(Uri.parse("android.resource://"+ context.getPackageName() +"/drawable/"+ zoneConfig.get("imageUrl")));
            holder.assetZoneDrop.setImageURI(Uri.parse("android.resource://"+ context.getPackageName() +"/drawable/"+ zoneConfig.get("imageUrl")));
            holder.assetZonePickup.setVisibility(zoneConfig.getInt("assetZonePickupVisibility"));
            holder.assetZoneDrop.setVisibility(zoneConfig.getInt("assetZoneDropVisibility"));
        }catch (Exception e){
            e.printStackTrace();
        }
    }

}
