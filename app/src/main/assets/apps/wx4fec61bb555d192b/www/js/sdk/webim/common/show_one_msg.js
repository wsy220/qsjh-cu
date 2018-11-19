var UserCancel = "取消通话";
var callError = "通话失败";
var conError = "连接失败";
var nobody = "无人接听";
var refuse = "对方拒绝";
var otherCancel = "对方结束通话";

//聊天页面增加一条消息
function addMsg(msg, prepend) {
	var isSelfSend, fromAccount, fromAccountNick, fromAccountImage, sessType, subType, sender;
	sender = "zs";
	//sender判断是不是自己发送的消息，自己发送
	//获取会话类型，目前只支持群聊
	//webim.SESSION_TYPE.GROUP-群聊，
	//webim.SESSION_TYPE.C2C-私聊，
	sessType = msg.getSession().type();
	//	console.log('SHOW_ONE_MESSAGEsessType ' + sessType);
	isSelfSend = msg.getIsSend(); //消息是否为自己发的
	//	console.log('isSelfSend ' + isSelfSend);
	fromAccount = msg.getFromAccount();
	//	console.log("fromAccount=>" + fromAccount);
	//	console.log('fromAccount ' + fromAccount);
	if(!fromAccount) {
		return;
	}
	if(isSelfSend) { //如果是自己发的消息
		sender = "self";
		//		if(webimLoginInfo.identifierNick) {
		//			fromAccountNick = webimLoginInfo.identifierNick;
		//		} else {
		//			fromAccountNick = fromAccount;
		//		}
		//		//获取头像
		//		userImages = serverAddress + '/' + $$.processImagePath(localStorage.getItem("HEAD_PHOTO"))
	}
	//	else { //如果别人发的消息
	//		//得到id --》fromAccount
	//		if(fromAccount == "admin") {
	//
	//		} else {
	//			var sussess = function(data) {
	//				if(data.obj) {
	//					userImages = serverAddress + "/" + data.obj.userfaceimage.replace('client', '').replace(/\\/g, "/");
	//				}
	//			}
	//			var showdetailUrl = serverAddress + "/api/doctor/showdetail/" + fromAccount;
	//			commonHttpUtils(showdetailUrl, "get", {}, sussess, error, true);
	//		}
	//
	//	}
	var datetime = webim.Tool.formatTimeStamp(msg.getTime());
	var now = moment().format("YYYY-MM-DD");
	var msgdate = moment(datetime).format("YYYY-MM-DD");
	var msgtime;
	if(now == msgdate) {
		msgtime = moment(datetime).format('HH:mm');
	} else {
		msgtime = datetime;
	}
	
	var time = document.createElement("div");
	var str= "text-align: center";
	time.setAttribute("style",str);
	time.innerHTML = "<span class='msg-time'>" + msgtime + "</span>"
	var onemsg = document.createElement("div");
	var msgimg = document.createElement("img");
	if(isSelfSend) {
		onemsg.className = "msg-item msg-item-self";
		msgimg.className = "msg-user msg-user-img"
	} else {
		onemsg.className = "msg-item";
		msgimg.className = "msg-user-img other-img";
		msgimg.alt = fromAccount;
	}
	//	msgimg.src = "../images/familydoctor/u125.png";
	var msgcontent = document.createElement("div");
	msgcontent.className = "msg-content";
	elems = msg.getElems();
	for(var i = 0; i < elems.length; i++) { //得到消息类型是文本还是图片或者表情
		elem = elems[i];
		type = elem.getType();
	}
	if(type == "TIMImageElem") {
		msgcontent.innerHTML = "<div class='msg-content-inner'><img class='msg-content-image' src=" + convertMsgtoHtml(msg) + " style='max-width: 100px;' /></div><div class='msg-content-arrow'></div>"
	} else {
		msgcontent.innerHTML = "<div class='msg-content-inner'>" + convertMsgtoHtml(msg) + "</div><div class='msg-content-arrow'></div>"
	}

	var msgclear = document.createElement("div");
	msgclear.className = "mui-item-clear";
	//	onemsg.appendChild(msghead);
	onemsg.appendChild(msgimg);
	onemsg.appendChild(msgcontent);
	onemsg.appendChild(msgclear);

	//消息列表
	var msgflow = document.getElementsByClassName("msgflow")[0];
	var scroller = document.querySelector('#scroller');
	if(prepend) {
		msgflow.insertBefore(onemsg, msgflow.firstChild);
		msgflow.insertBefore(time, msgflow.firstChild);
		setTimeout(function() {
			msgflow.scrollTop = 0;
		}, 300);
		bindImg();
		return;
	}

	//判断时间，当天则只取得时间
	if(type == "TIMCustomElem") {
		var content = convertMsgtoHtml(msg);
		console.log("content"+content)
		
		record.push({
			sender: sender,
			type: 'TIMCustomElem',
			content: content,
			time: msgtime,
			fromAccount: fromAccount
		});
		bindMsgList();
	}else if(type == "TIMImageElem") {
		record.push({
			sender: sender,
			type: 'image',
			content: convertMsgtoHtml(msg),
			time: msgtime,
			fromAccount: fromAccount
		});
		bindMsgList();
	} else {
		record.push({
			sender: sender,
			type: 'text',
			content: convertMsgtoHtml(msg),
			time: msgtime,
			fromAccount: fromAccount
		});
		bindMsgList();
	}
}

//把消息转换成Html
function convertMsgtoHtml(msg) {
	//	for(key in msg){
	//		console.log('!!convertMsgtoHtml!' + key + '=' + msg[key]);
	//	}
	//

	console.log("msg.getIsSend()=============》"+msg.getIsSend());
	if(msg.getIsSend()){
		sender = "self"
	}else{
		sender = "zs"
	}
	var html = "",
		elems, elem, type, content;
	elems = msg.getElems(); //获取消息包含的元素数组
	var count = elems.length;
	for(var i = 0; i < count; i++) {
		elem = elems[i];
		type = elem.getType(); //获取元素类型
		content = elem.getContent(); //获取元素对象
		//		console.log("消息类型" + type);
		//		console.log("消息类型1" + webim.MSG_ELEMENT_TYPE.TEXT);
		//		console.log("消息类型2" + webim.MSG_ELEMENT_TYPE.FACE);
		//		console.log("消息类型3" + webim.MSG_ELEMENT_TYPE.IMAGE);
		//		console.log("消息类型4" + webim.MSG_ELEMENT_TYPE.SOUND);
		//		console.log("消息类型5" + webim.MSG_ELEMENT_TYPE.FILE);
		//		console.log("消息类型6" + webim.MSG_ELEMENT_TYPE.CUSTOM);
		//		console.log("消息类型7" + webim.MSG_ELEMENT_TYPE.GROUP_TIP);
		switch(type) {
			case webim.MSG_ELEMENT_TYPE.TEXT:
				var eleHtml = convertTextMsgToHtml(content);
				//转义，防XSS
				html += webim.Tool.formatText2Html(eleHtml);
				break;
			case webim.MSG_ELEMENT_TYPE.FACE:
				html += convertFaceMsgToHtml(content);
				break;
			case webim.MSG_ELEMENT_TYPE.IMAGE:
				if(i <= count - 2) {
					var customMsgElem = elems[i + 1]; //获取保存图片名称的自定义消息elem
					var imgName = customMsgElem.getContent().getData(); //业务可以自定义保存字段，demo中采用data字段保存图片文件名
					html += convertImageMsgToHtml(content, imgName);
					i++; //下标向后移一位
				} else {
					html += convertImageMsgToHtml(content);
				}
				break;
			case webim.MSG_ELEMENT_TYPE.SOUND:
				html += convertSoundMsgToHtml(content);
				break;
			case webim.MSG_ELEMENT_TYPE.FILE:
				html += convertFileMsgToHtml(content);
				break;
			case webim.MSG_ELEMENT_TYPE.LOCATION:
				html += convertLocationMsgToHtml(content);
				break;
			case webim.MSG_ELEMENT_TYPE.CUSTOM:
			console.log("CUSTOM");
				var eleHtml = convertCustomMsgToHtml(content,sender);
				//转义，防XSS
				html += webim.Tool.formatText2Html(eleHtml);
				break;
			case webim.MSG_ELEMENT_TYPE.GROUP_TIP:
				var eleHtml = convertGroupTipMsgToHtml(content);
				//转义，防XSS
				html += webim.Tool.formatText2Html(eleHtml);
				break;
			default:
				webim.Log.error('未知消息元素类型: elemType=' + type);
				break;
		}
	}
	return html;
}

//解析文本消息元素
function convertTextMsgToHtml(content) {
	return content.getText();
}
//解析表情消息元素
function convertFaceMsgToHtml(content) {
	var faceUrl = null;
	var data = content.getData();
	var index = webim.EmotionDataIndexs[data];

	var emotion = webim.Emotions[index];
	if(emotion && emotion[1]) {
		faceUrl = emotion[1];
	}
	if(faceUrl) {
		return "<img src='" + faceUrl + "'/>";
	} else {
		return data;
	}
}
//解析图片消息元素
function convertImageMsgToHtml(content, imageName) {
	var smallImage = content.getImage(webim.IMAGE_TYPE.SMALL); //小图
	var bigImage = content.getImage(webim.IMAGE_TYPE.LARGE); //大图
	var oriImage = content.getImage(webim.IMAGE_TYPE.ORIGIN); //原图
	if(!bigImage) {
		bigImage = smallImage;
	}
	if(!oriImage) {
		oriImage = smallImage;
	}
	//	return "<img name='" + imageName + "' src='" + smallImage.getUrl() + "#" + bigImage.getUrl() + "#" + oriImage.getUrl() + "' style='CURSOR: hand' id='" + content.getImageId() + "' bigImgUrl='" + bigImage.getUrl() + "/>";
	return bigImage.getUrl();
}
//解析语音消息元素
function convertSoundMsgToHtml(content) {
	var second = content.getSecond(); //获取语音时长
	var downUrl = content.getDownUrl();
	if(webim.BROWSER_INFO.type == 'ie' && parseInt(webim.BROWSER_INFO.ver) <= 8) {
		return '[这是一条语音消息]demo暂不支持ie8(含)以下浏览器播放语音,语音URL:' + downUrl;
	}
	return '<audio id="uuid_' + content.uuid + '" src="' + downUrl + '" controls="controls" onplay="onChangePlayAudio(this)" preload="none"></audio>';
}
//解析文件消息元素
function convertFileMsgToHtml(content) {
	var fileSize, unitStr;
	fileSize = content.getSize();
	unitStr = "Byte";
	if(fileSize >= 1024) {
		fileSize = Math.round(fileSize / 1024);
		unitStr = "KB";
	}
	// return '<a href="' + content.getDownUrl() + '" title="点击下载文件" ><i class="glyphicon glyphicon-file">&nbsp;' + content.getName() + '(' + fileSize + unitStr + ')</i></a>';

	return '<a href="javascript:;" onclick=\'webim.onDownFile("' + content.uuid + '")\' title="点击下载文件" ><i class="glyphicon glyphicon-file">&nbsp;' + content.name + '(' + fileSize + unitStr + ')</i></a>';
}
//解析位置消息元素
function convertLocationMsgToHtml(content) {
	return '经度=' + content.getLongitude() + ',纬度=' + content.getLatitude() + ',描述=' + content.getDesc();
}
//解析自定义消息元素
function convertCustomMsgToHtml(content,sender) {
	console.log(content);
	console.log(sender);
	var data = content.getData(); // 类型
	var desc = content.getDesc(); // 时间
	var ext = content.getExt(); //扩展信息
	console.log("data--->"+data);
	if(sender !== "self"){//如果是对方的消息，加上“对方”前缀
		if(data == "对方忙"){
			data = "对方取消通话";
		}
		if(data == "取消通话"){
			data = "对方取消通话";
		}else if(data == "无人接听"){
			data = "对方取消通话";
		}else if(data == "对方拒绝"){
			data = "已拒绝";
		}
		else if(data == "结束通话"){
			data = "对方结束通话";
		}
		
	}

	var result = data;
	if(desc&&desc!=="00:00"){
		result = result+"  "+desc
	}
	return result;
}
//解析群提示消息元素
function convertGroupTipMsgToHtml(content) {
	var WEB_IM_GROUP_TIP_MAX_USER_COUNT = 10;
	var text = "";
	var maxIndex = WEB_IM_GROUP_TIP_MAX_USER_COUNT - 1;
	var opType, opUserId, userIdList;
	var groupMemberNum;
	opType = content.getOpType(); //群提示消息类型（操作类型）
	opUserId = content.getOpUserId(); //操作人id
	switch(opType) {
		case webim.GROUP_TIP_TYPE.JOIN: //加入群
			userIdList = content.getUserIdList();
			//text += opUserId + "邀请了";
			for(var m in userIdList) {
				text += userIdList[m] + ",";
				if(userIdList.length > WEB_IM_GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
					text += "等" + userIdList.length + "人";
					break;
				}
			}
			text = text.substring(0, text.length - 1);
			text += "加入该群，当前群成员数：" + content.getGroupMemberNum();
			break;
		case webim.GROUP_TIP_TYPE.QUIT: //退出群
			text += opUserId + "离开该群，当前群成员数：" + content.getGroupMemberNum();
			break;
		case webim.GROUP_TIP_TYPE.KICK: //踢出群
			text += opUserId + "将";
			userIdList = content.getUserIdList();
			for(var m in userIdList) {
				text += userIdList[m] + ",";
				if(userIdList.length > WEB_IM_GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
					text += "等" + userIdList.length + "人";
					break;
				}
			}
			text += "踢出该群";
			break;
		case webim.GROUP_TIP_TYPE.SET_ADMIN: //设置管理员
			text += opUserId + "将";
			userIdList = content.getUserIdList();
			for(var m in userIdList) {
				text += userIdList[m] + ",";
				if(userIdList.length > WEB_IM_GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
					text += "等" + userIdList.length + "人";
					break;
				}
			}
			text += "设为管理员";
			break;
		case webim.GROUP_TIP_TYPE.CANCEL_ADMIN: //取消管理员
			text += opUserId + "取消";
			userIdList = content.getUserIdList();
			for(var m in userIdList) {
				text += userIdList[m] + ",";
				if(userIdList.length > WEB_IM_GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
					text += "等" + userIdList.length + "人";
					break;
				}
			}
			text += "的管理员资格";
			break;

		case webim.GROUP_TIP_TYPE.MODIFY_GROUP_INFO: //群资料变更
			text += opUserId + "修改了群资料：";
			var groupInfoList = content.getGroupInfoList();
			var type, value;
			for(var m in groupInfoList) {
				type = groupInfoList[m].getType();
				value = groupInfoList[m].getValue();
				switch(type) {
					case webim.GROUP_TIP_MODIFY_GROUP_INFO_TYPE.FACE_URL:
						text += "群头像为" + value + "; ";
						break;
					case webim.GROUP_TIP_MODIFY_GROUP_INFO_TYPE.NAME:
						text += "群名称为" + value + "; ";
						break;
					case webim.GROUP_TIP_MODIFY_GROUP_INFO_TYPE.OWNER:
						text += "群主为" + value + "; ";
						break;
					case webim.GROUP_TIP_MODIFY_GROUP_INFO_TYPE.NOTIFICATION:
						text += "群公告为" + value + "; ";
						break;
					case webim.GROUP_TIP_MODIFY_GROUP_INFO_TYPE.INTRODUCTION:
						text += "群简介为" + value + "; ";
						break;
					default:
						text += "未知信息为:type=" + type + ",value=" + value + "; ";
						break;
				}
			}
			break;

		case webim.GROUP_TIP_TYPE.MODIFY_MEMBER_INFO: //群成员资料变更(禁言时间)
			text += opUserId + "修改了群成员资料:";
			var memberInfoList = content.getMemberInfoList();
			var userId, shutupTime;
			for(var m in memberInfoList) {
				userId = memberInfoList[m].getUserId();
				shutupTime = memberInfoList[m].getShutupTime();
				text += userId + ": ";
				if(shutupTime != null && shutupTime !== undefined) {
					if(shutupTime == 0) {
						text += "取消禁言; ";
					} else {
						text += "禁言" + shutupTime + "秒; ";
					}
				} else {
					text += " shutupTime为空";
				}
				if(memberInfoList.length > WEB_IM_GROUP_TIP_MAX_USER_COUNT && m == maxIndex) {
					text += "等" + memberInfoList.length + "人";
					break;
				}
			}
			break;
		default:
			text += "未知群提示消息类型：type=" + opType;
			break;
	}
	return text;
}