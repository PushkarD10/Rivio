/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import android.animation.ValueAnimator;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.graphics.PixelFormat;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.provider.Settings;
import android.text.Layout;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.RotateAnimation;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.Nullable;
import androidx.interpolator.view.animation.FastOutLinearInInterpolator;

import org.json.JSONException;
import org.json.JSONObject;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;


public class WidgetService extends Service {
    private View widgetView;
    private WindowManager windowManager;
    private ImageView imageClose;
    private float height, width;
    private String widgetMessage;
    private JSONObject entity_payload, data;


    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        showSilentNotification(intent);

//        addMessageToWidget(intent);
        return START_STICKY;
    }
    @Override
    public void onCreate() {
        super.onCreate();
        addWidgetToWindowManager();
    }

    private void showSilentNotification(Intent intent){
        try{

            // Fetch TextView for fare and distanceToPickup
            TextView fareTextView = widgetView.findViewById(R.id.ride_fare);
            TextView distanceTextView = widgetView.findViewById(R.id.distance_to_pickup);

            // Get Current Time in UTC
            final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
            f.setTimeZone(TimeZone.getTimeZone("UTC"));
            String getCurrTime = f.format(new Date());
            int calculatedTime =0;

            // Fetch data from intent
            if(intent!=null) {
                entity_payload = new JSONObject(intent.getStringExtra("payload"));
                String dataBuilder = intent.hasExtra("data") ? intent.getStringExtra("data") : null;
                data = dataBuilder!=null ? new JSONObject(dataBuilder) : null;
            }


            if(entity_payload!=null && entity_payload.has("baseFare")) {
                System.out.println("PAYLOAD + PAYLIAD " + entity_payload); // TODO:: REMOVE
                // Fetch data from entity_payload
                int fare = entity_payload.getInt("baseFare");
                int distanceToPickup = entity_payload.getInt("distanceToPickup");
                String searchRequestValidTill = entity_payload.getString("searchRequestValidTill");


                calculatedTime = calculateExpireTimer(searchRequestValidTill,getCurrTime);
                calculatedTime= calculatedTime > 30 ? 30 : calculatedTime;
                DecimalFormat df = new DecimalFormat();
                df.setMaximumFractionDigits(2);

                // Update text for fare and distanceToPickup
                fareTextView.setText("₹"+ fare);
                if(distanceToPickup>1000){
                    distanceTextView.setText((df.format(distanceToPickup/1000)) + " km pickup");
                }else {
                    distanceTextView.setText(distanceToPickup + " m pickup");
                }

                // Add silentRideRequest view
                View silentRideRequest = widgetView.findViewById(R.id.silent_ride_request_background);
                silentRideRequest.setVisibility(View.VISIBLE);

                // Start Slide-in animation
                silentRideRequest.setTranslationX(-1500);
                silentRideRequest.animate().translationX(0)
                        .setInterpolator(new FastOutLinearInInterpolator())
                        .setDuration(600)
                        .start();

                //onClick Listener for rideRequest
                silentRideRequest.setOnClickListener(view -> {
                    if(data!=null && entity_payload!=null) {
                        NotificationUtils.showAllocationNotification(getApplicationContext(), "", "", data, "", entity_payload);
                        silentRideRequest.setVisibility(View.GONE);
                    }
                });

                // Animate the floating widget
                View floatingWidget = widgetView.findViewById(R.id.floating_logo);
                float mAngleToRotate = 360f;
                rotationAnimation(floatingWidget, 0.0f, mAngleToRotate);

                //Revert Animation
                Handler handler = new Handler();
                handler.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        silentRideRequest.animate().translationX(-1500)
                                .setInterpolator(new FastOutLinearInInterpolator())
                                .setDuration(600)
                                .start();

                        rotationAnimation(floatingWidget, mAngleToRotate, 0.0f);

                        handler.postDelayed(new Runnable() {
                            @Override
                            public void run() {
                                silentRideRequest.setVisibility(View.GONE);
                            }
                        }, getResources().getInteger(R.integer.WIDGET_MESSAGE_ANIMATION_DURATION));
                    }
                }, calculatedTime*1000);
            }
        }catch (Exception e){

        }
    }

    private void rotationAnimation(View view, float start, float end){
        RotateAnimation wheelRotation = new RotateAnimation(start,end, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f);
        wheelRotation.setDuration(600);
        wheelRotation.setInterpolator(getApplicationContext(), android.R.interpolator.accelerate_decelerate);
        view.startAnimation(wheelRotation);
    }

    private int calculateExpireTimer(String expireTimeTemp, String currTimeTemp){
        String[] arrOfA = expireTimeTemp.split("T");
        String[] arrOfB = currTimeTemp.split("T");
        if(!arrOfA[0].equals(arrOfB[0])){
            return -1;
        }
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
        if ((expireTime-currTime) >= 5)
        {
            return expireTime-currTime - 5 ;
        }
        return 0;
    }

    private void addMessageToWidget(Intent intent){
        LinearLayout messageView;
        TextView messageTextView;
        TextView messageHeaderView = null;
        String widgetMessageHeader = null;
        if (intent!=null){
            widgetMessage = intent.getStringExtra(getResources().getString(R.string.WIDGET_MESSAGE));
            widgetMessageHeader = intent.getStringExtra("sentBy");
        }
        if (widgetMessage!=null){
            WindowManager.LayoutParams widgetLayoutParams =
                    new WindowManager.LayoutParams(
                            WindowManager.LayoutParams.MATCH_PARENT,
                            WindowManager.LayoutParams.WRAP_CONTENT,
                            android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O ?
                                    WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY :
                                    WindowManager.LayoutParams.TYPE_PHONE,
                            WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE,
                            PixelFormat.TRANSLUCENT);
            if (windowManager!=null){
//                if (widgetLayoutParams.x >= (windowManager.getDefaultDisplay().getWidth())/2){
                    messageView =  widgetView.findViewById(R.id.message_view_left);
                    messageTextView = widgetView.findViewById(R.id.messageTextView_left);
                // }else {
                //     messageView =  widgetView.findViewById(R.id.message_view_right);
                //     messageHeaderView = widgetView.findViewById(R.id.messageTextView_right_header);
                //     messageTextView = widgetView.findViewById(R.id.messageTextView_right);
                // }
                messageView.setVisibility(View.VISIBLE);
                messageTextView.setText(widgetMessage);
                if(widgetMessageHeader != null && messageHeaderView != null) {
                    messageTextView.setVisibility(View.VISIBLE);
                    messageHeaderView.setText(widgetMessageHeader);
                    messageHeaderView.setVisibility(View.VISIBLE);
                } else {
                    messageHeaderView.setVisibility(View.GONE);
                }
                Handler handler = new Handler();
//                handler.postDelayed(new Runnable() {
//                    @Override
//                    public void run() {
//                        Animation aniFade = AnimationUtils.loadAnimation(getApplicationContext(),R.anim.fade_out);
//                        messageView.startAnimation(aniFade);
//                        handler.postDelayed(new Runnable() {
//                            @Override
//                            public void run() {
//                                messageView.setVisibility(View.GONE);
//                            }
//                        }, getResources().getInteger(R.integer.WIDGET_MESSAGE_ANIMATION_DURATION));
//                    }
//                }, getResources().getInteger(R.integer.DURATION_OF_SHOWING_MESSAGE));
            }
        }
    }

    private void addWidgetToWindowManager() {
        if (!Settings.canDrawOverlays(this)) return;
        float scale = getResources().getDisplayMetrics().density;
        int LAYOUT_FLAG;
        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O){
            LAYOUT_FLAG = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
        }else{
            LAYOUT_FLAG = WindowManager.LayoutParams.TYPE_PHONE;
        }
        //inflating widgetView
        windowManager = (WindowManager) getSystemService(WINDOW_SERVICE);
        widgetView = LayoutInflater.from(this).inflate(R.layout.floating_widget_layout, null);
        WindowManager.LayoutParams widgetLayoutParams = new WindowManager.LayoutParams(WindowManager.LayoutParams.WRAP_CONTENT,WindowManager.LayoutParams.WRAP_CONTENT,LAYOUT_FLAG, WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE, PixelFormat.TRANSLUCENT);

        //initial Position
        widgetLayoutParams.gravity = Gravity.TOP | Gravity.LEFT;
        widgetLayoutParams.x = 5;
        widgetLayoutParams.y = (windowManager.getDefaultDisplay().getHeight())/4;

        //layout params for close button
        WindowManager.LayoutParams closeImageParams = new WindowManager.LayoutParams(WindowManager.LayoutParams.MATCH_PARENT, (int)(50*scale + 0.5f),LAYOUT_FLAG, WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE, PixelFormat.TRANSLUCENT);
        closeImageParams.gravity = Gravity.BOTTOM|Gravity.CENTER;

        imageClose = new ImageView(this);
        try{
            imageClose.setImageResource(R.drawable.ny_ic_close_transparent);
        }catch (Exception e){
            Log.e("Exception in rendering Image", e.toString());
        }
        imageClose.setPadding(0,0,0,(int)(10*scale + 0.5f));
        imageClose.setVisibility(View.GONE);// Removing this option
        imageClose.setBackground(this.getResources().getDrawable(R.drawable.widget_close_gradient));
        windowManager.addView(imageClose, closeImageParams);
        windowManager.addView(widgetView,widgetLayoutParams);
        widgetView.setVisibility(View.VISIBLE);
        height = windowManager.getDefaultDisplay().getHeight();
        width = windowManager.getDefaultDisplay().getWidth();


        //dragMovement
        widgetView.setOnTouchListener(new View.OnTouchListener() {
            int initialX, initialY;
            float initialTouchX, initialTouchY;
            boolean isCloseEnabled = false;
            long actionDownTime;
            @Override
            public boolean onTouch(View view, MotionEvent motionEvent) {
                try{
                    switch (motionEvent.getAction()){
                        case MotionEvent.ACTION_DOWN:
                            imageClose.setVisibility(View.GONE);
                            actionDownTime = Calendar.getInstance().getTimeInMillis();
                            initialX = widgetLayoutParams.x;
                            initialY = widgetLayoutParams.y;

                            //get touch position location
                            initialTouchX = motionEvent.getRawX();
                            initialTouchY = motionEvent.getRawY();
                            return true;

                        case MotionEvent.ACTION_UP:
                            imageClose.setVisibility(View.GONE);

                            if (isCloseEnabled){
//                                stopSelf();
                            } else {
                                ValueAnimator valueAnimator = ValueAnimator.ofFloat(widgetLayoutParams.x, 0);
                                valueAnimator.setDuration(getResources().getInteger(R.integer.WIDGET_CORNER_ANIMATION_DURATION));
                                valueAnimator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
                                    public void onAnimationUpdate(ValueAnimator animation) {
                                        try{
                                            widgetLayoutParams.x = Math.round((Float) animation.getAnimatedValue());
                                            windowManager.updateViewLayout(widgetView, widgetLayoutParams);
                                        }catch (Exception e){
                                            e.printStackTrace();
                                        }
                                    }
                                });
                                valueAnimator.start();

                                //click definition
                                if (Math.abs(initialTouchX - motionEvent.getRawX()) < 5 && Math.abs(initialTouchY - motionEvent.getRawY()) < 5){
                                    Intent intent = new Intent(getApplicationContext(), MainActivity.class);
                                    intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
                                    getApplicationContext().startActivity(intent);
//                                    stopSelf(); // Removed drag to close option for floating widget
                                }
                            }
                            return true;

                        case MotionEvent.ACTION_MOVE:
//                            if (Calendar.getInstance().getTimeInMillis() - actionDownTime>200){
//                                imageClose.setVisibility(View.GONE);
//                            }
                            widgetLayoutParams.x = initialX+ (int)(motionEvent.getRawX()- initialTouchX);
                            widgetLayoutParams.y = initialY+ (int)(motionEvent.getRawY()- initialTouchY);
                            windowManager.updateViewLayout(widgetView, widgetLayoutParams);

                            if (widgetLayoutParams.y > height*0.85 && widgetLayoutParams.x > width*0.30 && widgetLayoutParams.x < width*0.55){
                                imageClose.setImageResource(R.drawable.ny_ic_close_filled_round);
                                Vibrator vibrator = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);
                                if (!isCloseEnabled){
                                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                                        vibrator.vibrate(VibrationEffect.createOneShot(50, VibrationEffect.DEFAULT_AMPLITUDE));
                                    } else {
                                        //deprecated in API 26
                                        vibrator.vibrate(500);
                                    }
                                }
                                isCloseEnabled = true;
                            }else {
                                imageClose.setImageResource(R.drawable.ny_ic_close_transparent);
                                isCloseEnabled = false;
                            }
//                            ValueAnimator valueAnimator = ValueAnimator.ofFloat(widgetLayoutParams.x, 0);
//                            valueAnimator.setDuration(getResources().getInteger(R.integer.WIDGET_CORNER_ANIMATION_DURATION));
//                            valueAnimator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
//                                public void onAnimationUpdate(ValueAnimator animation) {
//                                    try{
//                                        widgetLayoutParams.x = Math.round((Float) animation.getAnimatedValue());
//                                        windowManager.updateViewLayout(widgetView, widgetLayoutParams);
//                                    }catch (Exception e){
//                                        e.printStackTrace();
//                                    }
//                                }
//                            });
//                            valueAnimator.start();
                            return true;
                    }
                }catch (Exception e){
                    e.printStackTrace();
                }
                return false;
            }
        });
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        if (widgetView!=null){
            windowManager.removeView(widgetView);
            widgetView = null;
        }
        if (imageClose!=null){
            windowManager.removeView(imageClose);
            imageClose = null;
        }
    }
    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}
