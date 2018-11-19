package com.qsjh.dcs.wxapi;

import android.app.Application;
import android.content.Context;
import android.util.Log;

import com.netease.nim.avchatkit.AVChatKit;
import com.netease.nim.avchatkit.config.AVChatOptions;
import com.netease.nimlib.sdk.AbortableFuture;
import com.netease.nimlib.sdk.NIMClient;
import com.netease.nimlib.sdk.ResponseCode;
import com.netease.nimlib.sdk.auth.AuthService;
import com.netease.nimlib.sdk.auth.LoginInfo;
import com.netease.nimlib.sdk.RequestCallbackWrapper;
import com.qsjh.config.QsjhCache;
import com.qsjh.config.preference.Preferences;

import io.dcloud.PandoraEntry;


public class Login {
    private AbortableFuture<LoginInfo> loginRequest;
    private String account;
    private String token ;

    public Login(String account,String token){
        this.account = account;
        this.token = token;
    }

    public void yxLogin(Context context){

        loginRequest = NIMClient.getService(AuthService.class).login(new LoginInfo(account, token));
        loginRequest.setCallback(new RequestCallbackWrapper() {
            @Override
            public void onResult(int code, Object result, Throwable exception) {
                Log.i("test", "real login, code=" + code);
                if (code == ResponseCode.RES_SUCCESS) {
                    //登录成功，保存登录信息
                    NimUIKitImpl.setAccount(account);
                    QsjhCache.setAccount(account);
                    Preferences.saveUserAccount(account);
                    Preferences.saveUserToken(token);
//                    MyApplication.context.initNimClient();
                }
            }
        });
    }
    public String getAccount(){
        return account;
    }
    public String getToken(){
        return token;
    }




}
