package com.qsjh.dcs.wxapi;

import com.netease.nimlib.sdk.uinfo.model.UserInfo;

import java.util.List;

/**
 * Created by hzchenkang on 2017/10/24.
 */

public interface IUserInfoProvider<T extends UserInfo> {

    /**
     * 同步获取userInfo
     *
     * @param account 账号
     * @return userInfo
     */
    T getUserInfo(String account);

    /**
     * 同步获取userInfo列表
     *
     * @param accounts 账号
     * @return userInfo
     */
    List<T> getUserInfo(List<String> accounts);


}
