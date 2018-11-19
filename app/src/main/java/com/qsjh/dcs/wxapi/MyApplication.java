package com.qsjh.dcs.wxapi;

import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;
import android.util.Log;

import com.netease.nim.avchatkit.AVChatKit;
import com.netease.nim.avchatkit.activity.AVChatActivity;
import com.netease.nim.avchatkit.application.AvchatkitApplication;
import com.netease.nim.avchatkit.config.AVChatOptions;
import com.netease.nim.avchatkit.model.IUserInfoProvider;
import com.netease.nimlib.sdk.AbortableFuture;
import com.netease.nimlib.sdk.NIMClient;
import com.netease.nimlib.sdk.RequestCallbackWrapper;
import com.netease.nimlib.sdk.ResponseCode;
import com.netease.nimlib.sdk.auth.AuthService;
import com.netease.nimlib.sdk.auth.LoginInfo;
import com.netease.nimlib.sdk.uinfo.model.UserInfo;
import com.netease.nimlib.sdk.util.NIMUtil;
import com.qsjh.config.QsjhCache;
import com.qsjh.config.preference.Preferences;

import java.util.List;

import io.dcloud.PandoraEntry;

import io.dcloud.application.DCloudApplication;
import io.dcloud.common.DHInterface.IWebview;
import io.dcloud.feature.internal.sdk.SDK;

public class MyApplication extends DCloudApplication implements AvchatkitApplication {

    @Override
    public void onCreate() {
        super.onCreate();

        QsjhCache.setContext(this);
//
//        // 4.6.0 开始，第三方推送配置入口改为 SDKOption#mixPushConfig，旧版配置方式依旧支持。
        NIMClient.init(this, getLoginInfo(), NimSDKOptionConfig.getSDKOptions(this));
        if (NIMUtil.isMainProcess(this)) {
            // 初始化音视频模块
            initAVChatKit();

        }


    }
    private LoginInfo getLoginInfo() {
        String account = Preferences.getUserAccount();
        String token = Preferences.getUserToken();

        if (!TextUtils.isEmpty(account) && !TextUtils.isEmpty(token)) {
            QsjhCache.setAccount(account.toLowerCase());
            return new LoginInfo(account, token);
        } else {
            return null;
        }
    }
    private void initAVChatKit() {
//        UserInfo userinfo = new UserInfo();

        AVChatOptions avChatOptions = new AVChatOptions(){
            @Override
            public void logout(Context context) {
                NIMClient.getService(AuthService.class).logout();
            }

        };
        avChatOptions.entranceActivity = PandoraEntry.class;//很关键
//        avChatOptions.notificationIconRes = R.drawable.ic_stat_notify_msg;
        AVChatKit.init(avChatOptions);
        // 设置用户相关资料提供者
//        AVChatKit.setUserInfoProvider(new IUserInfoProvider() {
//            @Override
//            public UserInfo getUserInfo(String account) {
//                return NimUIKit.getUserInfoProvider().getUserInfo(account);
//            }
//
//            @Override
//            public String getUserDisplayName(String account) {
//                return UserInfoHelper.getUserDisplayName(account);
//            }
//        });


    }


    public void exeJavaScrip(String stype, String msg,String timer) {
        // appid webviewId
        IWebview webview = SDK.obatinFirstPage(SDK.obtainCurrentApp());
        if(webview!= null){
            webview.evalJS("resultJsFunction('" + stype + "','" + msg + "','" + timer + "')");
        }
    }


    public void exeJavaScripWithCode(int code) {

//        this.exeJavaScrip(stype,msg,timer);
    }
}
