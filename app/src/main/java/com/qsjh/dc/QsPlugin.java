package com.qsjh.dc;

import io.dcloud.common.DHInterface.IWebview;
import io.dcloud.common.DHInterface.StandardFeature;
import io.dcloud.common.util.JSUtil;

import org.json.JSONArray;

import android.content.Intent;

import com.netease.nim.avchatkit.activity.AVChatActivity;
import com.netease.nimlib.sdk.NIMClient;
import com.netease.nimlib.sdk.auth.AuthService;
import com.qsjh.dcs.wxapi.Login;


public class QsPlugin extends StandardFeature {
    public void yxCallOut(IWebview pWebview, JSONArray array) {
        String CallBackID = array.optString(0);
        JSONArray newArray = new JSONArray();
        Intent intent = new Intent();
        for (int i = 1; i < array.length(); i++) {
            intent.putExtra("extra_" + i, array.optString(i));
        }
        intent.setClass(mApplicationContext, AVChatActivity.class); //设置跳转的Activity
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        mApplicationContext.startActivity(intent);
        JSUtil.execCallback(pWebview, CallBackID, newArray, JSUtil.OK, false);
    }

    public void yxLogout(IWebview pWebview, JSONArray array) {
        NIMClient.getService(AuthService.class).logout();
    }

    /**
     *  第一個参数 回调JS方法ID
     *  第二个参数 登录账号
     *  第三个参数 登录token
     * @param pWebview
     * @param array
     */
    public void yxLogin(IWebview pWebview, JSONArray array) {
        String CallBackID = array.optString(0);
        String account = array.optString(1);
        String token = array.optString(2);
        Login login =new Login(account,token);
        login.yxLogin(this.mApplicationContext);
    }



}
