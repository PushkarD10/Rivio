package in.juspay.mobility.app;

public interface CallBack {
    void customerCallBack(String notificationType);
    void driverCallBack(String notificationType);
    void imageUploadCallBack(String encImage, String filename, String filePath);
    void internetCallBack(String isPermission);
    void chatCallBack (String message,String sentBy, String time);
}