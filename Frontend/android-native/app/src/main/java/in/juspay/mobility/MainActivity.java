/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static in.juspay.mobility.BuildConfig.MERCHANT_TYPE;
import static in.juspay.mobility.app.Utils.minimizeApp;
import static in.juspay.mobility.app.Utils.setCleverTapUserProp;

import android.Manifest;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.ActivityManager;
import android.app.AlertDialog;
import android.app.NotificationChannelGroup;
import android.app.NotificationManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.Pair;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.webkit.WebView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.cardview.widget.CardView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;
import androidx.fragment.app.FragmentActivity;
import androidx.work.WorkManager;

import com.airbnb.lottie.LottieAnimationView;
import com.clevertap.android.pushtemplates.PushTemplateNotificationHandler;
import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.sdk.interfaces.NotificationHandler;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GoogleApiAvailability;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.Priority;
import com.google.android.gms.maps.MapsInitializer;
import com.google.android.gms.tasks.CancellationTokenSource;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.UpdateAvailability;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.firebase.perf.metrics.AddTrace;
import com.google.gson.Gson;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import java.util.Timer;
import java.util.UUID;
import java.util.Vector;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.mobility.app.ChatService;
import in.juspay.mobility.app.InAppNotification;
import in.juspay.mobility.app.LocationUpdateService;
import in.juspay.mobility.app.MobilityAppBridge;
import in.juspay.mobility.app.MyFirebaseMessagingService;
import com.google.firebase.crashlytics.FirebaseCrashlytics;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;
import in.juspay.mobility.app.RideRequestActivity;
import in.juspay.mobility.app.TranslatorMLKit;
import in.juspay.mobility.app.WidgetService;
import in.juspay.mobility.app.callbacks.ShowNotificationCallBack;
import in.juspay.mobility.app.reels.ExoplayerItem;
import in.juspay.mobility.app.services.MobilityAppUpdate;
import in.juspay.mobility.common.Utils;
import in.juspay.mobility.common.services.MobilityAPIResponse;
import in.juspay.mobility.common.services.MobilityCallAPI;
import in.juspay.services.HyperServices;

import static in.juspay.mobility.common.MobilityCommonBridge.isClassAvailable;

import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import java.util.Iterator;


import co.hyperverge.hyperkyc.HyperKyc;
import co.hyperverge.hyperkyc.data.models.HyperKycConfig;
import co.hyperverge.hyperkyc.data.models.result.HyperKycResult;
import in.juspay.hyper.core.BridgeComponents;



public class MainActivity extends AppCompatActivity {

    private static final String LOG_TAG = "MAIN_ACTIVITY";
    private static final int REQUEST_CODE_UPDATE_APP = 587;
    private static int updateType;
    MyFirebaseMessagingService.BundleUpdateCallBack bundleUpdateCallBack;
    private HyperServices hyperServices;

    private FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);

    private Context context;
    private Activity activity;
    @Nullable
    private SharedPreferences sharedPref;
    @SuppressLint("StaticFieldLeak")
    private static InAppNotification inAppNotification;
    ShowNotificationCallBack inappCallBack;
    private Future<JSONObject> driverInfoFutureTask;
    private Future<JSONObject> preInitFutureTask;
    private JSONObject currentLocationRes = new JSONObject();
    private JSONObject preInitFutureTaskResult = null;
    long onCreateTimeStamp = 0;
    private static final MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, true);
    ActivityResultLauncher<HyperKycConfig> launcher;
    private String registeredCallBackForHV;


    SharedPreferences.OnSharedPreferenceChangeListener mListener = new SharedPreferences.OnSharedPreferenceChangeListener() {
        @Override
        public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
            if (key != null && key.equals("LANGUAGE_KEY")) {
                new TranslatorMLKit("en",sharedPreferences.getString(key, "null"),MainActivity.this);
                Utils.updateLocaleResource(sharedPreferences.getString(key,"__failed"),context);
            }
            if (key != null && key.equals("REGISTERATION_TOKEN")) {
                String token = sharedPreferences.getString(key, "null");
                if (token.equals("__failed")) {
                    final PackageManager pm = getApplicationContext().getPackageManager();
                    final Intent intent = pm.getLaunchIntentForPackage(getApplicationContext().getPackageName());
                    try {
                        if (activity != null) {
                            activity.finishAffinity();// Finishes all activities.
                            activity.startActivity(intent);
                        } else {
                            sharedPreferences.edit().clear().apply();
                        }
                    } catch (NullPointerException e) {
                        e.printStackTrace();
                    }
                }
            }
            // Update Driver status in Local Storage
            if (key != null && key.equals("DRIVER_STATUS")) {
                String status = sharedPreferences.getString("DRIVER_STATUS", "null");
                WorkManager mWorkManager = WorkManager.getInstance(getApplicationContext());
                if (status.equals("null")) {
                    if (context != null) {
                        Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                        context.stopService(locationUpdateIntent);
                        mWorkManager.cancelAllWorkByTag(context.getString(in.juspay.mobility.app.R.string.location_update));
                    } else {
                        Context context = getApplicationContext();
                        Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                        context.stopService(locationUpdateIntent);
                        mWorkManager.cancelAllWorkByTag(context.getString(in.juspay.mobility.app.R.string.location_update));
                    }
                }
            }
            if (key != null && sharedPreferences.getString("DRIVER_STATUS", "null").equals("true") && (key.equals("RIDE_G_FREQUENCY") || key.equals("MAX_LIMIT_TO_STORE_LOCATION_PT") || key.equals("NO_OF_LOCATION_PT_TO_REMOVE") || key.equals("DRIVER_MIN_DISPLACEMENT") || key.equals("RIDE_T_FREQUENCY") || key.equals("TRIP_STATUS"))) {
                System.out.println("TRIGGERED UPDATE POLLING");
                Context context = getApplicationContext();
                Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                if(key.equals("TRIP_STATUS")){
                    locationUpdateIntent.putExtra("TRIP_STATUS", sharedPreferences.getString(key,"null"));
                }
                context.startService(locationUpdateIntent);
            }
        }
    };
    private Intent widgetService;
    private AppUpdateManager appUpdateManager;
    private boolean isHideSplashEventCalled = false;
    private boolean isSystemAnimEnabled = true;
    private String GAID;

    @Override
    public void onBackPressed() {
        if (hyperServices != null && !hyperServices.onBackPressed()) {
            super.onBackPressed();
        }
    }

    public String getAndUpdateRAMinSP() {
        String deviceRAM = "__failed";
        if (sharedPref != null) {
            deviceRAM = sharedPref.getString("DEVICE_RAM", "__failed");
        }
        if (!deviceRAM.equals("__failed"))
            return deviceRAM;
        long memory;
        try {
            memory = Utils.getDeviceRAM();
            deviceRAM = memory == 0 ? "null" : memory + " GB";
            sharedPref.edit().putString("DEVICE_RAM", deviceRAM).apply();
        } catch (Exception e) {
            System.out.println("In getDeviceRAM error: ");
            e.printStackTrace();
        }
        return deviceRAM;
    }

    public String[] getScreenDimensions() {
        String[] res = new String[0];
        if (sharedPref != null) {
            res = new String[]{sharedPref.getString("DEVICE_RESOLUTION", "__failed"), sharedPref.getString("DEVICE_SIZE", "__failed")};
        }
        if (!res[0].equals("__failed") && !res[1].equals("__failed"))
            return res;
        int height;
        int width;
        float size;
        try {
            DisplayMetrics displayMetrics = new DisplayMetrics();
            getWindowManager().getDefaultDisplay().getRealMetrics(displayMetrics);
            height = displayMetrics.heightPixels;
            width = displayMetrics.widthPixels;
            float x = height / displayMetrics.ydpi;
            float y = width / displayMetrics.xdpi;
            size = (float) Math.sqrt(x * x + y * y);
            size = (float) Math.round(size * 100) / 100;
            res[0] = height != 0 && width != 0 ? height + "x" + width + "px" : "null";
            res[1] = size != 0 ? size + " Inches" : "null";
            sharedPref.edit().putString("DEVICE_RESOLUTION", res[0]).apply();
            sharedPref.edit().putString("DEVICE_SIZE", res[1]).apply();
        } catch (Exception e) {
            System.out.println("In getScreenDimensions error: ");
            e.printStackTrace();
        }
        return res;
    }

    public String getDeviceDetails() {
        String deviceDetails = "";
        try {
            String bVersion = Build.VERSION.RELEASE;
            String bModel = Build.MODEL;
            String bBrand = Build.BRAND;
            String[] dim = getScreenDimensions();
            String deviceRAM = getAndUpdateRAMinSP();
            if (bModel == null || bModel.equals(""))
                bModel = "null";
            if (bBrand == null || bBrand.equals(""))
                bBrand = "null";
            bVersion = bVersion == null || bVersion.equals("") ? "null" : "Android v" + bVersion;
            deviceDetails = bBrand + "/" + bModel + "/" + bVersion + "/" + deviceRAM + "/" + dim[1] + "/" + dim[0];
        } catch (Exception e) {
            e.printStackTrace();
        }
        return deviceDetails;
    }

    private static HashMap<String, String> getQueryMap(String link) {
        String[] query_params_array = link.split("&");
        HashMap<String, String> query_params = new HashMap<>();
        for (String query_param : query_params_array) {
            String[] key_value = query_param.split("=");
            String key = key_value[0];
            String value = key_value[1];
            query_params.put(key, value);
        }
        return query_params;
    }

    private String getDriverProfile() {
        String baseUrl = in.juspay.mobility.BuildConfig.CONFIG_URL_DRIVER;
        String driverProfileUrl = baseUrl + "/driver/profile";
        try {
            MobilityCallAPI mobilityApiHandler = new MobilityCallAPI();
            Map<String, String> baseHeaders = mobilityApiHandler.getBaseHeaders(context);
            MobilityAPIResponse apiResponse = mobilityApiHandler.callAPI(driverProfileUrl, baseHeaders, null, "GET", false);
            return apiResponse.getResponseBody();
        } catch (Exception error) {
            Log.d(LOG_TAG, "Catch in getDriverProfile : " + error);
        }
        return null;
    }

    protected JSONObject preInitFlow() {
        Vector<String> res = handleDeepLinkIfAvailable(getIntent());
        Vector<String> notificationDeepLinkVector = notificationTypeHasDL(getIntent());

        String viewParam = null, deepLinkJson =null;
        if (res!=null ){
            viewParam = res.get(0);
            deepLinkJson = res.get(1);
        }
        else if (notificationDeepLinkVector != null) {
            viewParam = notificationDeepLinkVector.get(0);
            deepLinkJson = notificationDeepLinkVector.get(1);
        }

        if (MERCHANT_TYPE.equals("DRIVER")) {
            widgetService = new Intent(this, WidgetService.class);
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            if (sharedPref != null) {
                Utils.updateLocaleResource(sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.LANGUAGE_KEY), "null"),context);
            }
        }
        
        MobilityAppUpdate mobilityAppUpdate = new MobilityAppUpdate(this);
        mobilityAppUpdate.checkAndUpdateApp(remoteConfigs);

        updateConfigURL();
        mFirebaseAnalytics.setUserProperty("ct_objectId", Objects.requireNonNull(CleverTapAPI.getDefaultInstance(context)).getCleverTapID());

        JSONObject results = new JSONObject();
        try {
            if (viewParam != null) results.put("viewParam", viewParam);
            if (viewParam != null) results.put("view_param", viewParam);
            if (deepLinkJson != null) results.put("deepLinkJSON", deepLinkJson);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return results;
    }

    protected JSONObject getDriverInfoFlow() {
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");

        boolean shouldCallAPI = MERCHANT_TYPE.equals("DRIVER") && !token.equals("null") && !token.equals("__failed") && !token.equals("");

        JSONObject results = new JSONObject();
        try {
            if (shouldCallAPI) {
                String driverProfile = getDriverProfile();
                if (driverProfile != null) {
                    results.put("driverInfoResponse", new JSONObject(driverProfile));
                }
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
                return results;
    }

    protected void getCurrentLocationFlow() {
        if (ActivityCompat.checkSelfPermission(this, ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED || ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) return;
        FusedLocationProviderClient client = LocationServices.getFusedLocationProviderClient(this);
        CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
        client.getCurrentLocation(Priority.PRIORITY_HIGH_ACCURACY, cancellationTokenSource.getToken())
                .addOnSuccessListener(location -> {
                    if (location != null) {
                        try {
                            currentLocationRes.put("lat",location.getLatitude());
                            currentLocationRes.put("lon",location.getLongitude());
                        } catch (JSONException e) {
                            currentLocationRes = null;
                        }
                    }});
    }

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    @AddTrace(name = "onCreateTrace", enabled = true /* optional */)
    protected void onCreate(Bundle savedInstanceState) {
        Log.i("APP_PERF", "ON_CREATE_START : " + System.currentTimeMillis());
        onCreateTimeStamp = System.currentTimeMillis();
        super.onCreate(savedInstanceState);
        context = getApplicationContext();
        sharedPref = context.getSharedPreferences(this.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        activity = this;
        initiateHvLauncher();

        Log.i("APP_PERF", "FORKED_INIT_TASKS_AND_APIS : " + System.currentTimeMillis());        

        boolean isPerfEnabled = false, isPerfEnabledCustomer = false;
        try{
            isPerfEnabled = remoteConfigs.getBoolean("perf_enabled");
            isPerfEnabledCustomer = remoteConfigs.getBoolean("perf_enabled_customer");
            Log.i("PERF", "Fetched from remote config - perf enabled : " + isPerfEnabled);

        }catch(Exception e){
            Log.i("PERF", "unable to fetch PERF remote config");
            Exception exception = new Exception("Error in parsing perf config " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);            
        }

        if(isPerfEnabledCustomer){
            ExecutorService currentLocExecuter = Executors.newSingleThreadExecutor();
            currentLocExecuter.execute(() -> getCurrentLocationFlow());
        }

        if(isPerfEnabled) {
            preInitFutureTask = Executors.newSingleThreadExecutor().submit(this::preInitFlow);
            driverInfoFutureTask = Executors.newSingleThreadExecutor().submit(this::getDriverInfoFlow);
        } else {
            preInitFutureTaskResult = preInitFlow();
        }

        initApp();

        handleSplashScreen();

        WebView.setWebContentsDebuggingEnabled(true);

        boolean isMigrated = migrateLocalStore(context);
        String clientId = context.getResources().getString(R.string.client_id);

        mFirebaseAnalytics.logEvent(isMigrated ?"migrate_local_store_success" : "migrate_local_store_failed",new Bundle());
        initNotificationChannel();
        CleverTapAPI cleverTap = CleverTapAPI.getDefaultInstance(context);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CleverTapAPI.createNotificationChannel(context,clientId,"Promotion","Notifications Related to promotion",NotificationManager.IMPORTANCE_MAX, "4_promotional",true);
            CleverTapAPI.createNotificationChannel(context,"nammayatriHindi","nammayatriHindi","notification",NotificationManager.IMPORTANCE_MAX,true,"clevertap_custom_notification.mp3");
        }else{
            CleverTapAPI.createNotificationChannel(context,clientId,"Promotion","Notifications Related to promotion",NotificationManager.IMPORTANCE_MAX,true);
        }
        CleverTapAPI.setDebugLevel(CleverTapAPI.LogLevel.VERBOSE);
        cleverTap.enableDeviceNetworkInfoReporting(true);
        CleverTapAPI.setNotificationHandler((NotificationHandler)new PushTemplateNotificationHandler());


        sharedPref.edit().putString("DEVICE_DETAILS", getDeviceDetails()).apply();
        sharedPref.edit().putString("UNIQUE_DD", NotificationUtils.uniqueDeviceDetails()).apply();
        sharedPref.registerOnSharedPreferenceChangeListener(mListener);
        sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onCreate").apply();

        try {
            MapsInitializer.initialize(getApplicationContext());
        } catch (Exception e) {
            e.printStackTrace();
        }
        registerCallBack();
        inAppNotification = new InAppNotification(this);

        if (BuildConfig.DEBUG) {
            FirebaseMessaging.getInstance().subscribeToTopic("test");
        }
        Window window = this.getWindow();
        window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
        window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
        window.setStatusBarColor(this.getResources().getColor(R.color.colorPrimaryDark, getTheme()));
        countAppUsageDays();
    }

    public void initiateHvLauncher() {
        boolean enableHvLauncherRegistration = remoteConfigs.hasKey("enable_hv_launcher_registration")? remoteConfigs.getBoolean("enable_hv_launcher_registration") : true;
        if (enableHvLauncherRegistration && isClassAvailable ("co.hyperverge.hyperkyc.HyperKyc") && isClassAvailable("co.hyperverge.hyperkyc.data.models.result.HyperKycResult") && isClassAvailable("com.google.gson.Gson")) {
            launcher = this.registerForActivityResult(new HyperKyc.Contract(), new ActivityResultCallback<HyperKycResult>() {
                @Override
                public void onActivityResult(HyperKycResult result) {
                    try {
                        Gson gson = new Gson();
                        String jsonStr = gson.toJson(result);


                        JSONObject processPL = new JSONObject();
                        JSONObject innerPayload = getInnerPayload(new JSONObject(),"process_hv_resp");
                        innerPayload.put("callback", registeredCallBackForHV)
                                .put("hv_response", jsonStr);
                        processPL.put(PaymentConstants.PAYLOAD, innerPayload)
                                .put("requestId", UUID.randomUUID())
                                .put("service", getService());
                        hyperServices.process(processPL);
                    } catch (Exception e) {
                        Log.e("HV error : ", "error_in_HyperKycResult");
                    }

                }
            });
        }
    }
    private void handleSplashScreen() {
        try {
            setContentView(R.layout.activity_main);
            boolean skipDefaultSplash = false;
            String city = "__failed";
            if (sharedPref != null) {
                city = sharedPref.getString("DRIVER_LOCATION", "__failed");
                if (city.equals("__failed")) {
                    city = sharedPref.getString("CUSTOMER_LOCATION", "__failed");
                }
            }
            String merchantId = context.getResources().getString(R.string.merchant_id);
            JSONObject clevertapConfig = new JSONObject(remoteConfigs.getString("enable_city_based_splash_scn"));
            boolean enableCityBasedSplash = clevertapConfig.getBoolean(merchantId);
            View splash = findViewById(R.id.splash);
            LottieAnimationView splashLottie = splash.findViewById(R.id.splash_lottie);
            if (splashLottie != null) {
                if ((!city.equals("__failed")) && enableCityBasedSplash ) {
                    skipDefaultSplash = setSplashAnimAndStart(splashLottie, city.toLowerCase());
                }
                if ((!skipDefaultSplash) && enableCityBasedSplash) {
                    if ((splashLottie.getTag() != null) && splashLottie.getTag().equals("autoStart")) {
                        splashLottie.setVisibility(View.VISIBLE);
                        splashLottie.setRepeatCount(ValueAnimator.INFINITE);
                        splashLottie.playAnimation();
                    }
                }
                splash.setVisibility(View.VISIBLE);
            }
        } catch (Exception e){
            Bundle bundle = new Bundle();
            bundle.putString("Exception",e.toString());
            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
            mFirebaseAnalytics.logEvent("splash_screen_inflate_exception",bundle);
            setContentView(R.layout.activity_main_without_bg);
        }
    }

    private boolean setSplashAnimAndStart (LottieAnimationView view ,String city) {
        ResourceHandler resourceHandler = new ResourceHandler(this);
        @Nullable
        String animationFile = null;
        try {
            JSONObject cityConfig = getCityConfig(city);
            String file = cityConfig.optString("file_name","");
            if  (resourceHandler.isResourcePresent("raw",file) && !cityConfig.optBoolean("force_remote",false)) {
                animationFile = resourceHandler.getRawResource(file);
            } else {
                animationFile = cityConfig.optString("url");
            }
        } catch (Exception e) {
            Bundle bundle = new Bundle();
            bundle.putString("Exception",e.toString());
            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
            mFirebaseAnalytics.logEvent("exception_while_reading_city_config",bundle);
        }
        resourceHandler.close();
        if (animationFile != null && !animationFile.isEmpty()) {
            if (animationFile.startsWith("http")) {
                view.setFailureListener(throwable -> mFirebaseAnalytics.logEvent("failure_in_set_animation_from_url",new Bundle()));
                view.setAnimationFromUrl(animationFile);
            } else {
                view.setAnimationFromJson(animationFile,null);
            }
            view.setVisibility(View.VISIBLE);
            if ((view.getTag() != null) && view.getTag().equals("autoStart")) view.setRepeatCount(ValueAnimator.INFINITE);
            view.playAnimation();
            return true;
        } else {
            return false;
        }
    }

    @NonNull
    private boolean getCityConfigForFeatureFlags(String key) {
        MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, false);
        String city = sharedPref.getString("DRIVER_LOCATION", "__failed").toLowerCase();
        String forward_dispatch_config = remoteConfigs.getString(key);
        JSONObject config = new JSONObject();
        JSONObject cityConfig = new JSONObject();
        boolean isFeatureEnabled = false;
        Log.d("Feature flags","remote config for feature :-" + key +  "->" + forward_dispatch_config);
        try {
            config = new JSONObject(forward_dispatch_config);
            cityConfig = config.optJSONObject(city);
            Log.d("feature bool", "getCityConfigForFeatureFlags: " + cityConfig);
            if(cityConfig != null){isFeatureEnabled = cityConfig.optBoolean("is_" + key + "_Enabled", false);}
        } catch (Exception e) {
            Bundle bundle = new Bundle();
            bundle.putString("Exception",e.toString());
            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
            Log.d("feature bool deterimatal", "getCityConfigForFeatureFlags: " + cityConfig);
            mFirebaseAnalytics.logEvent("exception_while_reading_splash_config",bundle);
        }
        return isFeatureEnabled ;
    }

    @NonNull
    private JSONObject getCityConfig(String city) {
        MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, false);
        String splashScreenConfig = remoteConfigs.getString("splash_screen_" + city);
        JSONObject config = new JSONObject();
        JSONObject cityConfig = new JSONObject();
        try {
            cityConfig = new JSONObject(splashScreenConfig);
            String merchant = MERCHANT_TYPE.toLowerCase();
            config = cityConfig.optJSONObject(merchant);
        } catch (Exception e) {
            Bundle bundle = new Bundle();
            bundle.putString("Exception",e.toString());
            FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
            mFirebaseAnalytics.logEvent("exception_while_reading_splash_config",bundle);
        }
        return config != null ? config : cityConfig ;
    }

    private void registerCallBack() {
        inappCallBack = new ShowNotificationCallBack() {
            @Override
            public void showInAppNotification(JSONObject jsonObject, Context context) {
                showInAppNotificationApp(jsonObject, context);
            }

            @Override
            public void hideInAppNotification(String channelId) {
                hideInAppNotificationApp(channelId);
            }
        };
        ChatService.registerInAppCallback(inappCallBack);
        bundleUpdateCallBack = this::showAlertForUpdate;
        MyFirebaseMessagingService.registerBundleUpdateCallback(bundleUpdateCallBack);
        MyFirebaseMessagingService.registerShowNotificationCallBack(inappCallBack);
    }

    private void initNotificationChannel() {
        NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            try {
                notificationManager.deleteNotificationChannel("RINGING_ALERT");
                notificationManager.deleteNotificationChannel("TRIP_STARTED");
                notificationManager.deleteNotificationChannel("General");
                notificationManager.deleteNotificationChannel("FLOATING_NOTIFICATION");
                notificationManager.deleteNotificationChannel("DRIVER_QUOTE_INCOMING");
                notificationManager.deleteNotificationChannel("DRIVER_ASSIGNMENT");
                notificationManager.deleteNotificationChannel("REALLOCATE_PRODUCT");
                notificationManager.deleteNotificationChannel("GENERAL_NOTIFICATION");
                notificationManager.deleteNotificationChannel("RIDE_STARTED");
                notificationManager.deleteNotificationChannel("CANCELLED_PRODUCT");
                notificationManager.deleteNotificationChannel("DRIVER_HAS_REACHED");
                notificationManager.deleteNotificationChannel("TRIP_FINISHED");
                notificationManager.deleteNotificationChannel("SOS_TRIGGERED");
                notificationManager.deleteNotificationChannel("SOS_RESOLVED"); 
            } catch(Exception e) {
                System.out.println("Notification Channel doesn't exists");
            }
        }


        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannelGroup safetyGroup = new NotificationChannelGroup("1_safety", "Enhanced Safety");
            NotificationChannelGroup rideRelatedGroup = new NotificationChannelGroup("2_ride_related", "Essential - Ride related");
            NotificationChannelGroup serviceGroup = new NotificationChannelGroup("3_services", "Services");
            NotificationChannelGroup promotionalGroup = new NotificationChannelGroup("4_promotional", "Promotional");

            if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.P){
                safetyGroup.setDescription("Notifications related to Safety");
                rideRelatedGroup.setDescription("Notifications related to ride starts, end");
                serviceGroup.setDescription("Notifications related to Services");
                promotionalGroup.setDescription("Notifications related to promotional");
            }

            notificationManager.createNotificationChannelGroup(safetyGroup);
            notificationManager.createNotificationChannelGroup(rideRelatedGroup);
            notificationManager.createNotificationChannelGroup(serviceGroup);
            notificationManager.createNotificationChannelGroup(promotionalGroup);
        }


        NotificationUtils.createNotificationChannel(this, NotificationUtils.DRIVER_QUOTE_INCOMING);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.DRIVER_ASSIGNMENT);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.REALLOCATE_PRODUCT);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.GENERAL_NOTIFICATION);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.RIDE_STARTED);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.CANCELLED_PRODUCT);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.DRIVER_HAS_REACHED);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.SOS_TRIGGERED);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.SOS_RESOLVED);
    }

    public void updateConfigURL() {
        String key = MERCHANT_TYPE;
        String merchantId = key.equals("USER") ? in.juspay.mobility.BuildConfig.MERCHANT_ID_USER : in.juspay.mobility.BuildConfig.MERCHANT_ID_DRIVER;
        String baseUrl = key.equals("USER") ? in.juspay.mobility.BuildConfig.CONFIG_URL_USER : in.juspay.mobility.BuildConfig.CONFIG_URL_DRIVER;
        SharedPreferences sharedPreff = getApplicationContext().getSharedPreferences(
                activity.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPreff.edit();
        editor.putString("MERCHANT_ID", merchantId);
        editor.putString("BASE_URL", baseUrl);
        editor.apply();
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        mFirebaseAnalytics.logEvent("ny_hyper_onActivityResult",null);
        hyperServices.onActivityResult(requestCode, resultCode, data);
        if (requestCode == REQUEST_CODE_UPDATE_APP) {
            if (resultCode != RESULT_OK) {
                Log.i(LOG_TAG,"Update flow failed! Result code: " + resultCode);
                if(updateType == AppUpdateType.IMMEDIATE){
                    finishAndRemoveTask();
                }
            }
        }
    }

    private void initApp() {
        long initiateTimeStamp = System.currentTimeMillis();
        Log.i("APP_PERF", "INIT_APP_START : " + System.currentTimeMillis());
        hyperServices = new HyperServices(this, findViewById(R.id.cl_dui_container));
        Log.i("APP_PERF", "INIT_APP_HYPER_SERVICE_END : " + System.currentTimeMillis());
        final JSONObject json = new JSONObject();
        JSONObject payload = new JSONObject();

        try {
            json.put("requestId", UUID.randomUUID());
            json.put("service", getService());
            json.put("betaAssets", false);
            payload = getInnerPayload(payload,"initiate");
            payload.put("onCreateTimeStamp", onCreateTimeStamp);
            payload.put("initiateTimeStamp" , initiateTimeStamp);
            json.put(PaymentConstants.PAYLOAD, payload);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        mFirebaseAnalytics.logEvent("ny_hyper_initiate",null);
        Log.i("APP_PERF", "INIT_HYPER_SERVICE : " + System.currentTimeMillis());
        hyperServices.initiate(json, new HyperPaymentsCallbackAdapter() {
            @Override
            public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                Log.d(LOG_TAG, "onEvent: " + jsonObject.toString());
                String event = jsonObject.optString("event");
                mFirebaseAnalytics.logEvent("ny_hyper_" + event,null);
                switch (event) {
                    case "initiate_result":
                        Log.i("APP_PERF", "INITIATE_RESULT : " + System.currentTimeMillis());
                        try {
                            JSONObject innerPayload = json.getJSONObject(PaymentConstants.PAYLOAD);

                            String viewParam = null, deepLinkJSON = null;
                            JSONObject driverInfoResponse = null;
                            if(preInitFutureTaskResult != null) {
                                Log.i("APP_PERF", "PRE_INIT : " + System.currentTimeMillis());
                                viewParam = preInitFutureTaskResult.optString("viewParam");
                                deepLinkJSON = preInitFutureTaskResult.optString("deepLinkJson");
                            } else {
                                try {
                                    JSONObject preInitFutureTaskResult = preInitFutureTask.get(4500, TimeUnit.MILLISECONDS);
                                    Log.i("APP_PERF", "PRE_INIT_NO_EXCEPTION : " + System.currentTimeMillis());
                                    viewParam = preInitFutureTaskResult.optString("viewParam");
                                    deepLinkJSON = preInitFutureTaskResult.optString("deepLinkJson");
                                } catch (InterruptedException | ExecutionException | TimeoutException e) {
                                    preInitFutureTask.cancel(true);
                                    JSONObject preInitFutureTaskResult = preInitFlow();
                                    Log.i("APP_PERF", "PRE_INIT_EXCEPTION : " + System.currentTimeMillis());
                                    viewParam = preInitFutureTaskResult.optString("viewParam");
                                    deepLinkJSON = preInitFutureTaskResult.optString("deepLinkJson");
                                }
                                try {
                                    JSONObject driverInfoFutureTaskResult = driverInfoFutureTask.get(4500, TimeUnit.MILLISECONDS);
                                    driverInfoResponse = driverInfoFutureTaskResult.optJSONObject("driverInfoResponse");
                                    Log.i("APP_PERF", "PRE_INIT_CALL_API_NO_EXCEPTION : " + System.currentTimeMillis());
                                } catch (InterruptedException | ExecutionException | TimeoutException e) {
                                    driverInfoFutureTask.cancel(true);
                                    Log.i("APP_PERF", "PRE_INIT_CALL_API_EXCEPTION : " + System.currentTimeMillis());
                                    e.printStackTrace();
                                }
                            }
                            Log.i("APP_PERF", "INIT_FUTURE_TASK_RESULT : " + System.currentTimeMillis());

                            innerPayload.put("action", "process");
                            innerPayload.put("viewParam", viewParam);
                            innerPayload.put("view_param", viewParam);
                            innerPayload.put("deepLinkJSON", deepLinkJSON);
                            innerPayload.put("driverInfoResponse", driverInfoResponse);
                            innerPayload.put("currentLocation", currentLocationRes);

                            if (getIntent() != null) {
                                setNotificationData(innerPayload, getIntent());
                                handleGeoSchemeData(innerPayload, getIntent());
                            }
                            json.put(PaymentConstants.PAYLOAD, innerPayload);
                            mFirebaseAnalytics.logEvent("ny_hyper_process", null);
                            Log.i("APP_PERF", "INIT_HYPER_SERVICE_INITIATE_RESULT : " + System.currentTimeMillis());
                            hyperServices.process(json);
                        } catch (JSONException e) {
                            throw new RuntimeException(e);
                        }
                        break;
                    case "hide_loader":
                    case "hide_splash":
                        hideSplash();
                        break;
                    case "show_splash":
                        View v = findViewById(R.id.splash);
                        if (v != null) {
                            findViewById(R.id.splash).setVisibility(View.VISIBLE);
                        }
                        break;
                    case "reboot":
                        Log.i(LOG_TAG, "event reboot");
                        mFirebaseAnalytics.logEvent("ny_hyper_terminate",null);
                        hyperServices.terminate();
                        hyperServices = null;
                        initApp();
                        break;
                    case "in_app_notification":
                        showInAppNotificationApp(jsonObject, context);
                        break;
                    case "process_result":
                        try {
                            JSONObject innerPayload = jsonObject.getJSONObject(PaymentConstants.PAYLOAD);
                            if (innerPayload.getString("action").equals("terminate")) {
                                minimizeApp(context);
                            }
                        } catch (Exception ignored) {
                        }
                        break;
                    case "log_stream":
                        JSONObject payload;
                        try {
                            payload = jsonObject.getJSONObject("payload");
                            HashMap<String, String> params = new HashMap<>();
                            switch (payload.optString("label")) {
                                case "current_screen":
                                    params.put("screen_name", payload.getJSONObject("value").getString("screen_name"));
                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_current_screen", params ,context);
                                    break;
                                case "button_clicked":
                                    params.put("button_name",payload.getJSONObject("value").getString("button_name"));
                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_button_clicked",params ,context);
                                    break;
                                case "upi_apps":
                                    params.put("app_name",payload.getJSONObject("value").getString("appName"));
                                    params.put("package_name",payload.getJSONObject("value").getString("packageName"));
                                    in.juspay.mobility.app.Utils.logEventWithParams("ny_driver_payment_upi_app_selected",params ,context);
                                    break;
                                default:
                            }
                        } catch (JSONException e) {
                            Log.e(LOG_TAG, "empty payload" + json);
                        }
                        break;
                    case "launchHyperVerge":
                        try {
                            String cb = jsonObject.getString("callback");
                            registeredCallBackForHV = cb;
                            initHyperVergeSdk(jsonObject.getString("accessToken"), jsonObject.getString("workFlowId"), jsonObject.getString("transactionId"), jsonObject.getBoolean("useLocation"), jsonObject.getString("defLanguageCode"),jsonObject.getString("inputJson"));
                        }
                        catch (JSONException e) {
                            Log.e("Error Occurred while calling Hyperverge SDK ", e.toString());
                        }
                        break;

                    default:
                        Log.e(LOG_TAG, "json_payload" + json);
                }
            }
        });
        Log.i("APP_PERF", "INIT_HYPER_SERVICE_END : " + System.currentTimeMillis());
    }

    public void showAlertForUpdate() {
        System.out.println("inside showAlertForUpdate");
        AlertDialog.Builder builder = new AlertDialog.Builder(MainActivity.this);
        builder.setCancelable(false);
        ConstraintLayout constraintLayout = (ConstraintLayout) getLayoutInflater().inflate(in.juspay.mobility.app.R.layout.dynamic_update_loader, null);
        CardView cardView = constraintLayout.findViewById(in.juspay.mobility.app.R.id.apiLoaderOverlayCard);
        cardView.setCardElevation(0);
        cardView.setRadius(0);

        ViewGroup.LayoutParams layoutParams = new ConstraintLayout.LayoutParams(ConstraintLayout.LayoutParams.MATCH_PARENT, ConstraintLayout.LayoutParams.WRAP_CONTENT);
        constraintLayout.setLayoutParams(layoutParams);
        builder.setView(constraintLayout);
        builder.setPositiveButton(in.juspay.mobility.app.R.string.okay_got_it, (dialog, which) -> {
            dialog.cancel();
            mFirebaseAnalytics.logEvent("ny_hyper_terminate",null);
            hyperServices.terminate();
            hyperServices = null;
            initApp();
        });
        runOnUiThread(() -> {
            AlertDialog alertDialog = builder.create();
            alertDialog.show();
        });
    }


    protected Vector<String> handleDeepLinkIfAvailable(Intent appLinkIntent){
        if(appLinkIntent==null) return null;
        Vector<String> res = new Vector<>();
        Uri appLinkData = appLinkIntent.getData();
        String deepLinkJson = null, viewParam = null;
        if (appLinkData != null && appLinkData.getQuery() != null) {
            String query = appLinkData.getQuery();
            HashMap<String, String> query_params = getQueryMap(query);
            for (String key : query_params.keySet()) {
                if (key.equals("vp")){
                    viewParam = query_params.get(key);
                    break;
                } else if (key.equals("referrer")) {
                    viewParam = query;
                    break;
                }
            }
            Gson gson = new Gson();
            deepLinkJson = gson.toJson(query_params);
        } else return null;
        if(viewParam==null || deepLinkJson == null) return null;
        res.add(viewParam);
        res.add(deepLinkJson);
        return res;
    }

    private void processDeeplink(String viewParam, String deepLinkJson){
        try {
            JSONObject processPayloadDL = new JSONObject();
            JSONObject innerPayloadDL = getInnerPayload(new JSONObject(),"process");
            if (viewParam != null && deepLinkJson != null) {
                innerPayloadDL.put("view_param", viewParam)
                        .put("deepLinkJSON", deepLinkJson)
                        .put("viewParamNewIntent", viewParam)
                        .put("onNewIntent", true);
                processPayloadDL.put("service", getService())
                        .put("merchantId", getResources().getString(R.string.merchant_id))
                        .put("requestId", UUID.randomUUID())
                        .put(PaymentConstants.PAYLOAD, innerPayloadDL);
                mFirebaseAnalytics.logEvent("ny_hyper_process",null);
                hyperServices.process(processPayloadDL);
            }
        }catch (Exception e){
            // Need to handle exception
        }
    }

    private Vector<String> notificationTypeHasDL(Intent intent) {
        try {
            if (intent != null && intent.hasExtra("NOTIFICATION_DATA")) {
                String data = intent.getExtras().getString("NOTIFICATION_DATA");
                JSONObject jsonData = new JSONObject(data);
                if (jsonData.has("notification_type")){
                    String type = jsonData.getString("notification_type");
                    if (type.equals("COINS_SUCCESS")){
                        return new Vector<>(Arrays.asList("coins", "{\"vp\":\"coins\"}"));
                    }
                }
            }
        }catch (Exception e){

        }
        return null;
    }

    private Vector<Double> handleGeoSchemeVector(Intent intent){
        Uri data = intent.getData();
        Vector<Double> geoData = new Vector<>();
        String[] parts;
        try{
            if (data != null && data.getScheme().equals("geo")){
                parts = data.getSchemeSpecificPart().split(",");
                if (parts.length != 3) return geoData;
                String latitude = parts[0];
                String longitude = parts[2];
                geoData.add(Double.parseDouble(latitude));
                geoData.add(Double.parseDouble(longitude));
            }
        }catch (Exception e){
            e.printStackTrace();
        }

        return geoData;

    }
    private void handleGeoSchemeData(JSONObject innerPayload , Intent intent){
        try {
            Uri data = intent.getData();
            if (data != null && intent.getScheme().equals("geo")) {
                Vector<Double> geoData = handleGeoSchemeVector(intent);
                JSONObject geoObj = new JSONObject();
                geoObj.put("lat", geoData.get(0));
                geoObj.put("lon", geoData.get(1));
                geoObj.put("name",null);
                innerPayload.put("destination", geoObj);
            }
        }catch(Exception e){
            e.printStackTrace();
        }
    }
    @Override
    protected void onNewIntent(Intent intent) {
        Vector<String> res = handleDeepLinkIfAvailable(intent);
        Vector<String> notificationDeepLinkVector = notificationTypeHasDL(intent);
        String viewParam = null, deepLinkJson =null;
        if (res!=null ){
            viewParam = res.get(0);
            deepLinkJson = res.get(1);
        } else if (notificationDeepLinkVector != null) {
            viewParam = notificationDeepLinkVector.get(0);
            deepLinkJson = notificationDeepLinkVector.get(1);
        }
        processDeeplink(viewParam, deepLinkJson);
        if (intent != null && intent.hasExtra("NOTIFICATION_DATA")) {
            try {
                JSONObject proccessPayload = new JSONObject().put("service", getService())
                        .put("requestId", UUID.randomUUID());
                JSONObject innerPayload = new JSONObject().put("onNewIntent", true);
                proccessPayload.put(PaymentConstants.PAYLOAD, innerPayload);
                setNotificationData(innerPayload, intent);
                mFirebaseAnalytics.logEvent("ny_hyper_process",null);
                hyperServices.process(proccessPayload);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        if(intent.getScheme()!=null && intent.getScheme().equals("geo")){
            try{
                JSONObject proccessPayload = new JSONObject().put("service", getService())
                        .put("requestId", UUID.randomUUID());
                JSONObject innerPayload = new JSONObject().put("onNewIntent", true);
                proccessPayload.put(PaymentConstants.PAYLOAD, innerPayload);
                handleGeoSchemeData(innerPayload, intent);
                mFirebaseAnalytics.logEvent("ny_hyper_process",null);
                hyperServices.process(proccessPayload);
            }
            catch (Exception e){
                e.printStackTrace();
            }
        }
        super.onNewIntent(intent);
    }

    public void setNotificationData (JSONObject innerPayload, Intent intent) {
        try {
            String data = intent.getExtras().getString("NOTIFICATION_DATA");
            String fullNotificationString = intent.getExtras().getString("fullNotificationBody");
            JSONObject jsonData = new JSONObject(data);
            if (fullNotificationString != null) {
                JSONObject fullNotification = new JSONObject(fullNotificationString);
                innerPayload.put("fullNotificationBody", fullNotification);
            }
            if (jsonData.has("notification_type") && jsonData.getString("notification_type").equals("CHAT_MESSAGE")) {
                getInnerPayload(innerPayload, "OpenChatScreen");
                NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
                notificationManager.cancel(NotificationUtils.chatNotificationId);
                innerPayload.put("notification_type", "CHAT_MESSAGE");
            }
            if (jsonData.has("notification_type") && jsonData.has("entity_ids")) {
                String id = jsonData.getString("entity_ids");
                String type = jsonData.getString("notification_type");
                innerPayload.put("notification_type", type);
                if (type.equals("NEW_MESSAGE")) {
                    getInnerPayload(innerPayload, "callDriverAlert");
                    innerPayload.put("id", id)
                            .put("popType", type);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        setCleverTapUserProp("Session Status" , "true" , context);
        if (sharedPref != null) {
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onResume").apply();
            sharedPref.edit().putString("MAPS_OPENED", "null").apply();
        }
        if (appUpdateManager != null){
            appUpdateManager.getAppUpdateInfo().addOnSuccessListener(appUpdateInfo -> {
                if (appUpdateInfo.updateAvailability() == UpdateAvailability.DEVELOPER_TRIGGERED_UPDATE_IN_PROGRESS) {
                    // If an in-app update is already running, resume the update.
                    try {
                        appUpdateManager.startUpdateFlowForResult(
                                appUpdateInfo,
                                AppUpdateType.IMMEDIATE,
                                this,
                                REQUEST_CODE_UPDATE_APP
                        );
                    } catch (IntentSender.SendIntentException e) {
                        e.printStackTrace();
                    }
                }
            });
        }
        if (MERCHANT_TYPE.equals("DRIVER")) {
            if (NotificationUtils.overlayFeatureNotAvailable(this)) {
                checkRideRequest();
            }
            if (widgetService != null) {
                stopService(widgetService);
            }
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        pauseYoutubePlayer();
        setCleverTapUserProp("Session Status" , "false" , context);
        if (sharedPref != null)
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onPause").apply();
        if (BuildConfig.MERCHANT_TYPE.equals("DRIVER") &&
                widgetService != null && Settings.canDrawOverlays(this) &&
                !sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.REGISTERATION_TOKEN), "null").equals("null") &&
                !sharedPref.getString("DISABLE_WIDGET", "true").equals("true") &&
                !sharedPref.getString("ANOTHER_ACTIVITY_LAUNCHED", "false").equals("true")) {
            widgetService.putExtra("payload", "{}");
            widgetService.putExtra("data", "{}");
            startService(widgetService);
        }
    }

    @Override
    protected void onDestroy() {
        if (sharedPref != null) {
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onDestroy").apply();
        }
        pauseYoutubePlayer();
        if (hyperServices != null) {
            mFirebaseAnalytics.logEvent("ny_hyper_terminate",null);
            hyperServices.terminate();
        }
        ChatService.deRegisterInAppCallback(inappCallBack);
        MyFirebaseMessagingService.deRegisterBundleUpdateCallback(bundleUpdateCallBack);
        MyFirebaseMessagingService.deRegisterShowNotificationCallBack(inappCallBack);
        inAppNotification = null;
        super.onDestroy();
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        mFirebaseAnalytics.logEvent("ny_hyper_onRequestPermissionsResult",null);
        hyperServices.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    private void pauseYoutubePlayer(){
        MobilityAppBridge.youtubeVideoStatus = "PAUSE";
        if (MobilityAppBridge.youtubePlayer != null ) {
            MobilityAppBridge.youtubePlayer.pause();
        } else if (MobilityAppBridge.youTubePlayerView != null ) {
            MobilityAppBridge.youTubePlayerView = null;
        }
    }

    public void hideSplash() {
        View v = findViewById(in.juspay.mobility.app.R.id.cl_dui_container);
        if (v != null) {
            findViewById(in.juspay.mobility.app.R.id.cl_dui_container).setVisibility(View.VISIBLE);
        }
        View splashView = findViewById(R.id.splash);
        if (splashView != null) {
            splashView.setVisibility(View.GONE);
        }
    }

    private void countAppUsageDays() {
        Date currentDate = new Date();
        SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(this.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        long millis = sharedPref.getLong("PREVIOUS_USED_DATE", 0L);
        if (millis == 0L) {
            sharedPref.edit().putLong("PREVIOUS_USED_DATE", currentDate.getTime()).apply();
        }
        Date previousDate = new Date(sharedPref.getLong("PREVIOUS_USED_DATE", 0L));
        if (TimeUnit.MILLISECONDS.toDays(currentDate.getTime() - previousDate.getTime()) > 0) {
            // update days Count
            sharedPref.edit().putInt("DAYS_COUNT", sharedPref.getInt("DAYS_COUNT", 0) + 1).apply();
            sharedPref.edit().putString("USED_DAYS_COUNT", String.valueOf(sharedPref.getInt("DAYS_COUNT", 0))).apply();
            // update previousDate to currentDate
            sharedPref.edit().putLong("PREVIOUS_USED_DATE", currentDate.getTime()).apply();
        }
    }

    public void showInAppNotificationApp(JSONObject payload, Context context) {
        try {
            Handler handler = new Handler(context.getMainLooper());
            handler.postDelayed(() -> {
                try {
                    if (inAppNotification != null){
                        inAppNotification.generateNotification(payload);
                    }
                } catch (JSONException e) {
                    Log.e(LOG_TAG, "Error in In App Notification Handler " + e);
                }
            }, 0);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in In App Notification " + e);
        }
    }

    private void checkRideRequest() {
        try {
            boolean rideReqExpired = NotificationUtils.lastRideReq.getBoolean("rideReqExpired", true);
            if (rideReqExpired) return;
            Intent rideReqActivity = new Intent(this, RideRequestActivity.class);
            rideReqActivity.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
            rideReqActivity.putExtras(NotificationUtils.lastRideReq);
            startActivity(rideReqActivity);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in checkRideRequest");
        }
    }

    public void hideInAppNotificationApp (String channelId) {
        if (inAppNotification!= null) {
            inAppNotification.hideInAppNotification(channelId);
        }
    }
    private class GetGAIDTask extends AsyncTask<String, Integer, String> {
        @Override
        protected String doInBackground(String... strings) {
            AdvertisingIdClient.Info adInfo;
            adInfo = null;
            try {
                if(GoogleApiAvailability.getInstance().isGooglePlayServicesAvailable(MainActivity.this.getApplicationContext()) != ConnectionResult.SUCCESS) {
                    return "google play service not available";
                }
                adInfo = AdvertisingIdClient.getAdvertisingIdInfo(MainActivity.this.getApplicationContext());
                if (adInfo.isLimitAdTrackingEnabled()) // check if user has opted out of tracking
                    return "did not found GAID... sorry";
            } catch (IOException | GooglePlayServicesRepairableException |
                     GooglePlayServicesNotAvailableException e) {
                e.printStackTrace();
            }
            return adInfo != null ? adInfo.getId() : "did not found GAID... sorry";
        }
        @Override
        protected void onPostExecute(String s) {
            GAID = s;
            System.out.println("GAID "+GAID);
            Bundle params = new Bundle();
            params.putString("id",GAID);
            FirebaseAnalytics.getInstance(context).logEvent("ad_id", params);
        }
    }

    public String getService() {
        if (MERCHANT_TYPE.equals("USER")) {
            return "in.yatri.consumer";
        } else {
            return "in.yatri.provider";
        }
    }

    private boolean migrateLocalStore(Context context) {
        SharedPreferences oldSharedPref = context.getSharedPreferences("namma_yatri_app_local_keys",MODE_PRIVATE);
        SharedPreferences currentSharedPref = context.getSharedPreferences(context.getString(in.juspay.mobility.app.R.string.preference_file_key),MODE_PRIVATE);
        Map<String,?> oldEntries = oldSharedPref.getAll();
        for (Map.Entry<String, ?> entry : oldEntries.entrySet()) {
            Object current = entry.getValue();
            if (current instanceof Integer) {
                currentSharedPref.edit().putInt(entry.getKey(),(int)current).apply();
            } else if (current instanceof String) {
                currentSharedPref.edit().putString(entry.getKey(),(String) current).apply();
            }else if (current instanceof Float) {
                currentSharedPref.edit().putFloat(entry.getKey(),(float) current).apply();
            }else if (current instanceof Long) {
                currentSharedPref.edit().putLong(entry.getKey(),(long) current).apply();
            }else if (current instanceof Boolean) {
                currentSharedPref.edit().putBoolean(entry.getKey(),(boolean) current).apply();
            }
        }
        oldSharedPref.edit().clear().apply();
        return true;
    }

    private JSONObject getInnerPayload(JSONObject payload, String action) throws JSONException{
        String appName = "";
        try{
            appName = context.getApplicationInfo().loadLabel(context.getPackageManager()).toString();
        }catch (Exception e){
            e.printStackTrace();
        }
        payload.put("clientId", getResources().getString(R.string.client_id));
        payload.put("merchantId", getResources().getString(R.string.merchant_id));
        payload.put("appName", appName);
        payload.put("action", action);
        payload.put("logLevel",1);
        payload.put("isBootable",true);
        payload.put(PaymentConstants.ENV, "prod");
        int bundleTimeOut = Integer.parseInt(KeyValueStore.read(this,getString(in.juspay.mobility.app.R.string.preference_file_key),"BUNDLE_TIME_OUT","500"));
        payload.put("bundleTimeOut",bundleTimeOut);
        return payload;
    }

    public void initHyperVergeSdk(String accessToken,  String workFlowId, String transactionId, boolean useLocation, String defLanguageCode, String inputsJson) {
        if (isClassAvailable ("co.hyperverge.hyperkyc.data.models.HyperKycConfig")) {
                HyperKycConfig config = new HyperKycConfig(accessToken, workFlowId, transactionId);
                config.setUseLocation(useLocation);
                config.setDefaultLangCode(defLanguageCode);
                if (inputsJson.length() > 0) {
                    Map<String, String> inpMap = new HashMap<>();
                    JSONObject jsonObject;
                    try {
                        jsonObject = new JSONObject(inputsJson);
                        for (Iterator<String> it = jsonObject.keys(); it.hasNext(); ) {
                            String key = it.next();
                            inpMap.put(key, jsonObject.getString(key));
                        }
                    }
                    catch (JSONException e) {
                        Log.e("Unable find Specified Key, So returning config without setting inputs.", inputsJson);
                        e.printStackTrace();
                        return;
                    }
                    if (inpMap.size() > 0)  config.setInputs(inpMap);
                    else Log.d("HyperKycConfig Inputs JSON: ", "Empty json passed as input so not initializing inputs in config");
                }
                else Log.d("HyperKycConfig Inputs JSON: ", "Not initializing inputs as inputs json passed is null");
                launcher.launch(config);
            } else {

        }
        }

}