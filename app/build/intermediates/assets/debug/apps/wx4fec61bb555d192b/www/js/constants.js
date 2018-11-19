//服务器地址
//var serverAddress = "http://192.168.0.88:3000";
var serverAddress = "http://192.168.0.6:3000";
//var serverAddress = "http://pt.70jiahu.com";
//var serverAddress = "https://tpt.70jiahu.com";
//var serverAddress = "http://192.168.0.11:3000";
//var serverAddress = "http://192.168.0.13:3000";
//首页轮播链接配置
var dataConfig_imglunbo = {
	"MSG001": {
		"name": "优惠券广告链接",
		"code": "1",
		"type": "sm",
		"link": "/geren_message/d07_my_account.html",
		"opentype": "1",
		"introduction": "优惠券广告链接",
	},
	"MSG002": {
		"name": "关于我们链接",
		"code": "1",
		"type": "sm",
		"link": "/geren_message/info.html",
		"opentype": "2",
		"introduction": "关于我们链接",
	},
	"MSG003": {
		"name": "外部链接",
		"code": "1",
		"type": "sm",
		"link": "/geren_message/info.html",
		"opentype": "-1",
		"introduction": "外部链接",
	}
}

//原声TITLE样式配置

var styleTITLE = {};

function returnstyleTitle(link, title) {
	styleTITLE = {
		titleNView: { // 窗口的标题栏控件
			titleText: title, // 标题栏文字,当不设置此属性时，默认加载当前页面的标题，并自动更新页面的标题
			titleColor: "#FFFFFF", // 字体颜色,颜色值格式为"#RRGGBB",默认值为"#000000"
			titleSize: "15px", // 字体大小,默认17px
			backgroundColor: "#3fccd2", // 控件背景颜色,颜色值格式为"#RRGGBB",默认值为"#F7F7F7"
			progress: { // 标题栏控件的进度条样式
				color: "#00FF00", // 进度条颜色,默认值为"#00FF00"  
				height: "2px" // 进度条高度,默认值为"2px"         
			},
			splitLine: { // 标题栏控件的底部分割线，类似borderBottom
				color: "#CCCCCC", // 分割线颜色,默认值为"#CCCCCC"  
				height: "1px" // 分割线高度,默认值为"2px"
			},
			buttons: [{
				fontSrc: "_www/fonts/mui.ttf",
				text: '\ue471',
				float: 'left',
				fontSize: "24px",
				onclick: function() {
					plus.webview.getWebviewById(link).close(true);
				}
			}]
		}
	}
	return styleTITLE;
}

/**
 * 依赖MUI
 * @param {Object} url
 * @param {Object} dataType
 * @param {Object} data
 * @param {Object} sussess
 * @param {Object} error
 */

function commonHttpUtils(url, dataType, data, success, error, async) {
	var reqUrl = url;
	console.log(reqUrl);
	access_token = localStorage.getItem("TOKENMD5");
	var successDate;
	if(access_token) {
		successDate = function(resData) {
			//console.log("sssserror"+JSON.stringify(resData))
			if(resData.result == "error" && resData.msg == "TOKEN_ERROR") {
				mui.alert("登录过期，请重新登录");
				//setTimeout(appRestart(), 2000);
				localStorage.clear();
			} else if(resData.result == "error" && resData.msg == "USER_NOT_EXIST") {
				mui.alert("登录过期，请重新登录");
				//setTimeout(appRestart(), 2000);
				localStorage.clear();
			} else {
				success(resData)
			}
		};
	} else {
		successDate = success;
	}
	mui.ajax(reqUrl, {
		data: data,
		crossDomain: true,
		dataType: 'json', //服务器返回json格式数据
		type: dataType, //HTTP请求类型
		timeout: 30000, //超时时间设置为30秒；
		beforeSend: function() {
			plus.nativeUI.showWaiting();
		},
		complete: function() {
			plus.nativeUI.closeWaiting();
		},
		async: async,
		headers: {
			"Access-Control-Allow-Headers": "X-Requested-With",
			"x-access-token": access_token
		},
		beforeSend: function() {
			plus.nativeUI.showWaiting();
		},
		complete: function() {
			plus.nativeUI.closeWaiting();
		},
		//		success: success,
		success: successDate,
		error: function(xhr, type, errorThrown) {
			if(error) {
				error(xhr, type, errorThrown);
			}
		}
	});
}

function commonHttpUtils2(url, dataType, data, success, error, async) {
	var reqUrl = url;
	access_token = localStorage.getItem("TOKENMD5");
	mui.ajax(reqUrl, {
		data: data,
		dataType: 'json', //服务器返回json格式数据
		type: dataType, //HTTP请求类型
		timeout: 30000, //超时时间设置为30秒；
		async: async,
		headers: {
			"Access-Control-Allow-Headers": "X-Requested-With",
			"x-access-token": access_token
		},
		success: success,
		//		success: function(resData) {
		//			//console.log("sssserror"+JSON.stringify(resData))
		//			if(resData.result == "error" && resData.msg == "TOKEN_ERROR") {
		//				alert("登录过期，请重新登录");
		//				setTimeout(appRestart(), 2000);
		//			} else if(resData.result == "error" && resData.msg == "USER_NOT_EXIST") {
		//				alert("登录过期，请重新登录");
		//				setTimeout(appRestart(), 2000);
		//			} else {
		//				success(resData)
		//			}
		//		},
		error: error
	});
}

function commonHttpUtilsLUNBO(url, dataType, data, success, error, async, isxianshi) {
	var reqUrl = url;
	access_token = localStorage.getItem("TOKENMD5");
	mui.ajax(reqUrl, {
		data: data,
		dataType: 'json', //服务器返回json格式数据
		type: dataType, //HTTP请求类型
		timeout: 30000, //超时时间设置为30秒；
		async: async,
		headers: {
			"Access-Control-Allow-Headers": "X-Requested-With",
			"x-access-token": access_token
		},
		success: success,
		error: function(xhr, type, errorThrown) {
			if(error) {
				error(xhr, type, errorThrown);
			}
		}
	});
}

function commonHttpUtilsLOGOUT(url, dataType, data, success, error, async) {
	var reqUrl = url;
	access_token = localStorage.getItem("TOKENMD5");
	mui.ajax(reqUrl, {
		data: data,
		dataType: 'json', //服务器返回json格式数据
		type: dataType, //HTTP请求类型
		timeout: 30000, //超时时间设置为30秒；
		async: async,
		headers: {
			"Access-Control-Allow-Headers": "X-Requested-With",
			"x-access-token": access_token
		},
		success: success,
		error: error
	});
}

function commonHttpUtilsNoWaiting(url, httpReqType, data, success, errors, complete) {
	var reqUrl = url;
	var access_token = localStorage.getItem("TOKENMD5");
	console.log(reqUrl);
	mui.ajax(reqUrl, {
		data: data,
		dataType: 'json', //服务器返回json格式数据
		type: httpReqType, //HTTP请求类型
		timeout: 30000, //超时时间设置为180秒
		headers: {
			"Access-Control-Allow-Headers": "X-Requested-With",
			"x-access-token": access_token
		},
		//		complete: function() {
		//			if(complete) {
		//				complete();
		//			}
		//		},
		success: function(resData) {
			//console.log("sssserror"+JSON.stringify(resData))
			if(resData.result == "error" && resData.msg == "TOKEN_ERROR") {
				mui.alert("登录过期，请重新登录");
				//setTimeout(appRestart(), 2000);
				localStorage.clear();
			} else if(resData.result == "error" && resData.msg == "USER_NOT_EXIST") {
				mui.alert("登录过期，请重新登录");
				//setTimeout(appRestart(), 2000);
				localStorage.clear();
			} else {
				success(resData)
			}
		},
		error: function(xhr, type, errorThrown) {
			if(error) {
				error(xhr, type, errorThrown);
			}
		}
	});
}

/**
 * 通用错误处理
 * @param {Object} xhr
 * @param {Object} type
 * @param {Object} errorThrown
 */
var error = function(xhr, type, errorThrown, url) {
	console.log("error1 " + xhr.status);
	console.log("error2 " + xhr.readyState);
	console.log("error3 " + type);
	console.log("error4 " + url);
	//异常处理；
	if(xhr.status != 500) {
		mui.toast('公共网络请求超时，请稍后再试！');
	}
	//	if(!isxianshi) {
	//		document.querySelector(".mui-content").innerHTML='<div class="mui-content-padded wutu"><img src="../images/no_data/1-01.png" style="width: 150px;"></div>';
	//	}
}

//formatHttpString = function(str) {
//	if(str != null && str != '' && (str.indexOf("client\\") == 0)) {
//		return serverAddress + '/' + str.replace('client', '').replace(/\\/g, "\\");
//	}
//	if(str != null && str != '' && (str.indexOf("client/") == 0)) {
//		return serverAddress + '/' + str.replace('client', '').replace(/\\/g, "/");
//	}
//
//	if(str == "") {
//		str = "../images/wode/txmrt.png";
//		return str;
//	}
//	if(str.indexOf("client\\") < 0) {
//		str = "../images/wode/txmrt.png";
//		return str;
//	}
//
//}

var formatHttpString = function(str) {
	if(str == "") {
		str = "../images/wode/txmrt.png";
		return str;
	}
	if(str && str.indexOf("client") < 0) {
		str = serverAddress + str;
		return str;
	}
	if(str != null && str != '' && (str.indexOf("client\\") == 0)) {
		return serverAddress + '/' + str.replace('client', '').replace(/\\/g, "\\");

	}
	if(str != null && str != '' && (str.indexOf("client/") == 0)) {
		return serverAddress + '/' + str.replace('client', '').replace(/\\/g, "/");
	}

}

formatHttpStringHc = function(str) {
	var reg = new RegExp("\n", "g");
	return str.replace(reg, '<br>');
}
formatHttpStringAT = function(str) {
	var reg = new RegExp("\n", "g");
	if(str.indexOf('\\') == -1) {
		return str.replace("\\", "");
	} else {
		return str.replace(reg, '<br>').replace(/\s/g, "&nbsp");
	}
}

function judgePlatform() {
	switch(plus.os.name) {
		case "Android":
			// Android平台: plus.android.*
			break;
		case "iOS":
			// iOS平台: plus.ios.*
			break;
		default:
			// 其它平台
			break;
	}
}

function appRestart() {
	localStorage.clear();
	plus.runtime.restart();
}

function JSONLength(obj) {
	var size = 0,
		key;
	for(key in obj) {
		if(obj.hasOwnProperty(key)) size++;
	}
	return size;
};


function formatJE(jiage) {
	return(jiage / 100).toFixed(2);
}
function formatJENO2(jiage) {
	return(jiage / 100);
}
function download_img2222(image_url) {
	var image_md5 = md5(image_url);
	var local_image_url = '_downloads/image/' + image_md5 + '.jpg'; // 缓存本地图片url

	//	alert(image_url);
	//	alert(local_image_url);
	var download_task = plus.downloader.createDownload(image_url, {
		filename: local_image_url // filename:下载任务在本地保存的文件路径
	}, function(download, status) {
		//alert(local_image_url);

	});
	download_task.start();
	return local_image_url;
};

function isCardID(sId) {
	var iSum = 0;
	var info = "";
	if(!/^\d{17}(\d|x)$/i.test(sId)) return "你输入的身份证长度或格式错误";
	sId = sId.replace(/x$/i, "a");
	if(aCity[parseInt(sId.substr(0, 2))] == null) return "你的身份证地区非法";
	sBirthday = sId.substr(6, 4) + "-" + Number(sId.substr(10, 2)) + "-" + Number(sId.substr(12, 2));
	var d = new Date(sBirthday.replace(/-/g, "/"));
	if(sBirthday != (d.getFullYear() + "-" + (d.getMonth() + 1) + "-" + d.getDate())) return "身份证上的出生日期非法";
	for(var i = 17; i >= 0; i--) iSum += (Math.pow(2, i) % 11) * parseInt(sId.charAt(17 - i), 11);
	if(iSum % 11 != 1) return "你输入的身份证号非法";
	//aCity[parseInt(sId.substr(0,2))]+","+sBirthday+","+(sId.substr(16,1)%2?"男":"女");//此次还可以判断出输入的身份证号的人性别
	return true;
};

function GetAge(idCard) {
	var birthday = "";
	if(idCard != null && idCard != "") {
		if(idCard.length == 15) {
			birthday = "19" + idCard.substr(6, 6);
		} else if(idCard.length == 18) {
			birthday = idCard.substr(6, 8);
		}

		birthday = birthday.replace(/(.{4})(.{2})/, "$1-$2-");
		birthday = birthday.split("-")[0];
	}

	return birthday;
};
document.addEventListener("plusready", function() {
	// 声明的JS“扩展插件别名”
	var _BARCODE = 'qsPlugin',
		B = window.plus.bridge;
	var qsPlugin = {
		// 声明异步返回方法
		yxCallOut: function(Argus1, Argus2, Argus3, Argus4, successCallback, errorCallback) {
			var success = typeof successCallback !== 'function' ? null : function(args) {
					successCallback(args);
				},
				fail = typeof errorCallback !== 'function' ? null : function(code) {
					errorCallback(code);
				};
			callbackID = B.callbackId(success, fail);
			// 通知Native层plugintest扩展插件运行”PluginTestFunction”方法
			return B.exec(_BARCODE, "yxCallOut", [callbackID, Argus1, Argus2, Argus3, Argus4]);
		},
		yxLogin: function(Argus1, Argus2, Argus3, Argus4, successCallback, errorCallback) {
			var success = typeof successCallback !== 'function' ? null : function(args) {
					successCallback(args);
				},
				fail = typeof errorCallback !== 'function' ? null : function(code) {
					errorCallback(code);
				};
			callbackID = B.callbackId(success, fail);
			// 通知Native层plugintest扩展插件运行”PluginTestFunction”方法
			return B.exec(_BARCODE, "yxLogin", [callbackID, Argus1, Argus2, Argus3, Argus4]);
		},
		yxLogout: function(Argus1, Argus2, Argus3, Argus4, successCallback, errorCallback) {
			var success = typeof successCallback !== 'function' ? null : function(args) {
					successCallback(args);
				},
				fail = typeof errorCallback !== 'function' ? null : function(code) {
					errorCallback(code);
				};
			callbackID = B.callbackId(success, fail);
			// 通知Native层plugintest扩展插件运行”PluginTestFunction”方法
			return B.exec(_BARCODE, "yxLogout", [callbackID, Argus1, Argus2, Argus3, Argus4]);
		},
	};
	window.plus.qsPlugin = qsPlugin;
}, true);

//getBirthdayFromIdCard: function(idCard) {
//	var birthday = "";
//	if(idCard != null && idCard != "") {
//		if(idCard.length == 15) {
//			birthday = "19" + idCard.substr(6, 6);
//		} else if(idCard.length == 18) {
//			birthday = idCard.substr(6, 8);
//		}
//
//		birthday = birthday.replace(/(.{4})(.{2})/, "$1-$2-");
//	}
//
//	return birthday;
//}

