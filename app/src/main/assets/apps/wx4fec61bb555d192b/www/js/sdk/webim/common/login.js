function tlsGetUserSig(res) {
	//成功拿到凭证
	if(res.ErrorCode == webim.TLS_ERROR_CODE.OK) {
		//从当前URL中获取参数为identifier的值
		loginInfo.identifier = webim.Tool.getQueryString("identifier");
		//拿到正式身份凭证
		loginInfo.userSig = res.UserSig;
		//从当前URL中获取参数为sdkappid的值
		loginInfo.sdkAppID = loginInfo.appIDAt3rd = Number(webim.Tool.getQueryString("sdkappid"));
		//从cookie获取accountType
		var accountType = webim.Tool.getCookie('accountType');
		if(accountType) {
			loginInfo.accountType = accountType;
			//sdk登录
			webimLogin();

		} else {
			alert('accountType非法');
		}
	} else {
		//签名过期，需要重新登录
		if(res.ErrorCode == webim.TLS_ERROR_CODE.SIGNATURE_EXPIRATION) {
			tlsLogin();
		} else {
//			alert("[" + res.ErrorCode + "]" + res.ErrorInfo);
			alert("签名过期");
		}
	}
}
//sdk登录
//接收聊天代码
//帐号模式，0-表示独立模式，1-表示托管模式
var accountMode = 0;

//官方 demo appid,需要开发者自己修改（托管模式）
var sdkAppID = 1400053640;
var accountType = 20035;
var userSIG = localStorage.getItem("adminSig");
userSIGxiaoming = localStorage.getItem("userSig");
var AdminAcount = 'qwe101';
selType = webim.SESSION_TYPE.C2C; //当前聊天类型
selToID = null; //当前选中聊天id（当聊天类型为私聊时，该值为好友帐号，否则为群号）
var selSess = null; //当前聊天会话对象
var recentSessMap = {}; //保存最近会话列表
var reqRecentSessCount = 50; //每次请求的最近会话条数，业务可以自定义
infoMap = {}; //初始化时，可以先拉取我的好友和我的群组信息

//默认好友头像
friendHeadUrl = '../images/001.png'; //仅demo使用，用于没有设置过头像的好友
//默认群头像
groupHeadUrl = '../images/001.png'; //仅demo使用，用于没有设置过群头像的情况

webimLoginInfo = {
	'sdkAppID': sdkAppID, //用户所属应用id,必填
	'identifier': localStorage.getItem("userID"), //当前用户ID,必须是否字符串类型，必填
	'accountType': accountType, //用户所属应用帐号类型，必填
	'userSig': userSIGxiaoming, //当前用户身份凭证，必须是字符串类型，必填
	'identifierNick': localStorage.getItem("GEREN_NAME"), //当前用户昵称，不用填写，登录接口会返回用户的昵称，如果没有设置，则返回用户的id
	'headurl': '../images/70jiahu.png' //当前用户默认头像，选填，如果设置过头像，则可以通过拉取个人资料接口来得到头像信息
};
//监听连接状态回调变化事件
var onConnNotify = function(resp) {
	var info;
	switch(resp.ErrorCode) {
		case webim.CONNECTION_STATUS.ON:
			console.log('建立连接成功: ' + resp.ErrorInfo);
			break;
		case webim.CONNECTION_STATUS.OFF:
			info = '连接已断开，无法收到新消息，请检查下你的网络是否正常: ' + resp.ErrorInfo;
			console.log(info);
			break;
		case webim.CONNECTION_STATUS.RECONNECT:
			info = '连接状态恢复正常: ' + resp.ErrorInfo;
			console.log(info);
			break;
		default:
			console.log('未知连接状态: =' + resp.ErrorInfo);
			break;
	}
};

//IE9(含)以下浏览器用到的jsonp回调函数
function jsonpCallback(rspData) {
	webim.setJsonpLastRspData(rspData);
}

var listeners = {
	"onConnNotify": onConnNotify, //监听连接状态回调变化事件,必填
	"jsonpCallback": jsonpCallback, //IE9(含)以下浏览器用到的jsonp回调函数，
	"onMsgNotify": onMsgNotify //监听新消息(私聊，普通群(非直播聊天室)消息，全员推送消息)事件，必填
};
