package com.qsjh.dcs.wxapi;

import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.util.Log;

import com.netease.nimlib.sdk.AbortableFuture;
import com.netease.nimlib.sdk.NIMClient;
import com.netease.nimlib.sdk.RequestCallback;
import com.netease.nimlib.sdk.auth.AuthService;
import com.netease.nimlib.sdk.auth.LoginInfo;
import com.netease.nimlib.sdk.msg.attachment.MsgAttachment;
import com.netease.nimlib.sdk.msg.constant.SessionTypeEnum;
import com.netease.nimlib.sdk.msg.model.IMMessage;
import com.netease.nimlib.sdk.team.constant.TeamTypeEnum;
import com.netease.nimlib.sdk.team.model.Team;

/**
 * UIKit能力实现类。
 */
public final class NimUIKitImpl {
    // context
    private static Context context;

    // 自己的用户帐号
    private static String account;

    // 用户信息提供者
    private static IUserInfoProvider userInfoProvider;



    public static void loginSuccess(String account) {
        setAccount(account);
    }

    public static void logout() {
//        DataCacheManager.clearDataCache();
//        ChatRoomCacheManager.clearCache();
//        getImageLoaderKit().clear();
//        LoginSyncDataStatusObserver.getInstance().reset();
    }

    // 初始化用户信息提供者
    private static void initUserInfoProvider(IUserInfoProvider userInfoProvider) {

//        if (userInfoProvider == null) {
//            userInfoProvider = new DefaultUserInfoProvider();
//        }

        NimUIKitImpl.userInfoProvider = userInfoProvider;
    }


    public static IUserInfoProvider getUserInfoProvider() {
        return userInfoProvider;
    }

    public static void setAccount(String account) {
        NimUIKitImpl.account = account;
    }

    /*
    * ****************************** basic ******************************
    */
    public static Context getContext() {
        return context;
    }

    public static String getAccount() {
        return account;
    }
}
