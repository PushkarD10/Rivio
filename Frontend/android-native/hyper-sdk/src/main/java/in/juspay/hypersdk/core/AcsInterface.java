package in.juspay.hypersdk.core;

import android.util.Base64;
import android.webkit.JavascriptInterface;

import androidx.annotation.NonNull;
import com.caoccao.javet.annotations.V8Function;
import androidx.annotation.Nullable;

/**
 * Created by sahil on 13/03/17.
 */

public class AcsInterface {

    @NonNull
    private final DynamicUI dui;
    @Nullable
    private final DuiInterface duiInterface;

    public AcsInterface(JuspayServices juspayServices) {
        this.dui = juspayServices.getDynamicUI();
        this.duiInterface = juspayServices.getJBridge();
    }

    @JavascriptInterface @V8Function
    public void invoke(String methodName, String argumentsJson, String callbackFunctionName) {
        if (isFunctionAllowedToInvoke(methodName)) {
            String encoded = Base64.encodeToString(argumentsJson.getBytes(), Base64.NO_WRAP);
            String command = String.format("window[\"onEvent'\"]('%s',atob('%s'),'%s')", methodName, encoded, callbackFunctionName);

            dui.addJsToWebView(command);
        }
    }

    @JavascriptInterface @V8Function
    public void invoke(String methodName, String argumentsJson) {
        if (isFunctionAllowedToInvoke(methodName)) {
            String encoded = Base64.encodeToString(argumentsJson.getBytes(), Base64.NO_WRAP);
            String command = String.format("window[\"onEvent'\"]('%s',atob('%s'))", methodName, encoded);
            dui.addJsToWebView(command);
        }
    }

    private boolean isFunctionAllowedToInvoke(String functionName) {
        return functionName.matches("^[a-zA-Z0-9]*$");
    }

    @JavascriptInterface @V8Function
    public boolean isOnline() {
        if (duiInterface != null) {
            return duiInterface.isOnline();
        }
        return true;
    }

    @JavascriptInterface @V8Function
    public String getDataFromSharedPrefs(String key) {
        if (duiInterface != null) {
            return duiInterface.getDataFromSharedPrefs(key, "");
        }
        return "__failed";
    }

    @JavascriptInterface @V8Function
    public String getSessionAttribute(String key) {
        if (duiInterface != null) {
            return duiInterface.getSessionAttribute(key, "");
        }
        return "__failed";
    }

    @JavascriptInterface @V8Function
    public String getSessionInfo() {
        String result = "";
        if (duiInterface != null) {
            result = duiInterface.getSessionInfo();
        }
        return result.equals("") ? "__failed" : result;
    }

    @JavascriptInterface @V8Function
    public String getResourceByName(String key) {
        if (duiInterface != null) {
            return duiInterface.getResourceByName(key);
        }
        return "__failed";
    }

}
