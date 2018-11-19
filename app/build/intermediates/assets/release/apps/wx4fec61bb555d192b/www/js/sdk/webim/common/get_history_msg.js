//获取历史消息(c2c或者group)成功回调函数
//msgList 为消息数组，结构为[Msg]
function getHistoryMsgCallback(selToID, msgList, prepage) {
	//	var selToID = selToID;
	var msg;
	prepage = prepage || false;
	//如果是加载前几页的消息，消息体需要prepend，所以先倒排一下
	if(prepage) {
		console.log("prepage===>"+prepage);
		msgList.reverse();
	}
	
	for(var j in msgList) { //遍历新消息
		msg = msgList[j];
		//		console.log(msg.getSession().id());
		if(msg.getSession().id() == selToID) { //为当前聊天对象的消息
			console.log("为当前聊天对象的消息");
			selSess = msg.getSession();
			//在聊天窗体中新增一条消息
			addMsg(msg, prepage);
		}
	}

	//消息已读上报，并将当前会话的消息设置成自动已读
	webim.setAutoRead(selSess, true, true);
}

//获取最新的c2c历史消息,用于切换好友聊天，重新拉取好友的聊天消息
var getLastC2CHistoryMsgs = function(selToID, cbOk, cbError) {
	//	console.log("getLastC2CHistoryMsgs");
	if(selType == webim.SESSION_TYPE.GROUP) {
		alert('当前的聊天类型为群聊天，不能进行拉取好友历史消息操作');
		return;
	}
	console.log("selToID===?"+selToID);
	if(!selToID || selToID == '@TIM#SYSTEM') {
		alert('当前的聊天id非法，selToID=' + selToID);
		return;
	}
	var timestamp = Math.round(new Date().getTime() / 1000) + 100000000; //得到当前时间戳
	//	console.log("时间戳" + timestamp);
	var lastMsgTime = timestamp; //第一次拉取好友历史消息时，必须传0
	//	var unixTimestamp = new Date(lastMsgTime* 1000);
	//	var commonTime = unixTimestamp.toLocaleString();
	//	console.log("获取当前时间"+commonTime);
	var msgKey = '';

	var options = {
		'Peer_Account': selToID, //好友帐号
		'MaxCnt': 10, //拉取消息条数
		'LastMsgTime': lastMsgTime, //最近的消息时间，即从这个时间点向前拉取历史消息
		'MsgKey': msgKey
	};
	selSess = null;
	webim.MsgStore.delSessByTypeId(selType, selToID);

	webim.getC2CHistoryMsgs(
		options,
		function(resp) {
			var complete = resp.Complete; //是否还有历史消息可以拉取，1-表示没有，0-表示有
			if(resp.MsgList.length == 0) {
				webim.Log.warn("没有历史消息了:data=" + JSON.stringify(options));
				return;
			}
			for(var respkey in resp) {
				//				console.log('!!resp!!' + respkey + '=' + resp[respkey]);
			} //*/
			var list = resp.MsgList;

			//			console.log("-------------------------------------------------------------------------------"+list.length)
			for(var n = 0; n < list.length; n++) {
				var msg = list[n];
				//				console.log("msg.getType()" + msg.getType());
				//				for(var key in msg) {
				//					console.log('!!getC2CHistoryMsgs!' + key + '=' + msg[key]);
				//				}
				addMsg(msg);
			}

			getPrePageC2CHistroyMsgInfoMap[selToID] = { //保留服务器返回的最近消息时间和消息Key,用于下次向前拉取历史消息
				'LastMsgTime': resp.LastMsgTime,
				'MsgKey': resp.MsgKey
			};
			//清空聊天界面
			//document.getElementsByClassName("msgflow")[0].innerHTML = "";
			if(cbOk)
				cbOk(resp.MsgList);
		},
		cbError
	);
};

//向上翻页，获取更早的好友历史消息
var getPrePageC2CHistoryMsgs = function(cbOk, cbError) {
	if(selType == webim.SESSION_TYPE.GROUP) {
//		alert('当前的聊天类型为群聊天，不能进行拉取好友历史消息操作');
		getPrePageGroupHistoryMsgs();
		return;
	}
	console.log("selToID===>" + selToID);
	var tempInfo = getPrePageC2CHistroyMsgInfoMap[selToID]; //获取下一次拉取的c2c消息时间和消息Key
	console.log("tempInfo--->>>>"+tempInfo);
	var lastMsgTime;
	var msgKey;
	if(tempInfo) {
		lastMsgTime = tempInfo.LastMsgTime;
		msgKey = tempInfo.MsgKey;
	} else {
		alert('没有历史消息了');
		return;
	}
	var options = {
		'Peer_Account': selToID, //好友帐号
		'MaxCnt': 5, //拉取消息条数
		'LastMsgTime': lastMsgTime, //最近的消息时间，即从这个时间点向前拉取历史消息
		'MsgKey': msgKey
	};
	webim.getC2CHistoryMsgs(
		options,
		function(resp) {
			var complete = resp.Complete; //是否还有历史消息可以拉取，1-表示没有，0-表示有
			if(resp.MsgList.length == 0) {
					mui.toast("没有历史消息了");
				return;
			}
			var list = resp.MsgList;
			console.log("-------------------------------------------------------------------------------"+list.length)
			for(var n = 0; n < list.length; n++) {
				var msg = list[n];
				console.log("msg.getType()" + msg.getType());
//				addMsg(msg);
			}
			getPrePageC2CHistroyMsgInfoMap[selToID] = { //保留服务器返回的最近消息时间和消息Key,用于下次向前拉取历史消息
				'LastMsgTime': resp.LastMsgTime,
				'MsgKey': resp.MsgKey
			};
			if(cbOk) {//调用的时候不传cbok则直接进else
				cbOk(resp.MsgList);
			} else {
				console.log('获取更多历史记录的 聊天对象的id=>'+selToID);
				getHistoryMsgCallback(selToID,resp.MsgList, true);
			}
		},
		cbError
	);
};

//获取最新的群历史消息,用于切换群组聊天时，重新拉取群组的聊天消息
var getLastGroupHistoryMsgs = function(cbOk) {
	//	console.log("getLastGroupHistoryMsgs--selToID="+selToID);
	//	console.log("getLastGroupHistoryMsgs--selType="+selType);
	if(selType == webim.SESSION_TYPE.C2C) {
		alert('当前的聊天类型为好友聊天，不能进行拉取群历史消息操作');
		return;
	}
	getGroupInfo(selToID, function(resp) {
		//		console.log("getGroupInfo-selToID===>"+selToID);
		//拉取最新的群历史消息
		var options = {
			'GroupId': selToID,
			'ReqMsgSeq': resp.GroupInfo[0].NextMsgSeq - 1,
			'ReqMsgNumber': 10
		};
		if(options.ReqMsgSeq == null || options.ReqMsgSeq == undefined || options.ReqMsgSeq <= 0) {
			mui.toast("没有历史消息了");
			return;
		}
		selSess = null;
		webim.MsgStore.delSessByTypeId(selType, selToID);
		recentSessMap[webim.SESSION_TYPE.GROUP + "_" + selToID] = {};

		recentSessMap[webim.SESSION_TYPE.GROUP + "_" + selToID].MsgGroupReadedSeq = resp.GroupInfo && resp.GroupInfo[0] && resp.GroupInfo[0].MsgSeq;
		webim.syncGroupMsgs(
			options,
			function(msgList) {
				console.log("$$msgList.length+>" + msgList.length);
				if(msgList.length == 0) {
					mui.toast("没有历史消息了");
					return;
				}
				var msgSeq = msgList[0].seq - 1;
				getPrePageGroupHistroyMsgInfoMap[selToID] = {
					"ReqMsgSeq": msgSeq
				};
				//清空聊天界面
				document.getElementsByClassName("msgflow")[0].innerHTML = "";
				if(cbOk)
					cbOk(msgList);
				getHistoryMsgCallback(selToID, msgList)
			},
			function(err) {
				console.log(err.ErrorInfo);
			}
		);
	});
};

//向上翻页，获取更早的群历史消息
var getPrePageGroupHistoryMsgs = function(cbOk) {
	if(selType == webim.SESSION_TYPE.C2C) {
		alert('当前的聊天类型为好友聊天，不能进行拉取群历史消息操作');
		return;
	}
	var tempInfo = getPrePageGroupHistroyMsgInfoMap[selToID]; //获取下一次拉取的群消息seq
	var reqMsgSeq;
	if(tempInfo) {
		reqMsgSeq = tempInfo.ReqMsgSeq;
		if(reqMsgSeq <= 0) {
			mui.toast("没有历史消息了");
			return;
		}
	} else {
		webim.Log.error('获取下一次拉取的群消息seq为空');
		return;
	}
	var options = {
		'GroupId': selToID,
		'ReqMsgSeq': reqMsgSeq,
		'ReqMsgNumber': 10
	};
	webim.syncGroupMsgs(
		options,
		function(msgList) {
			console.log("msgList.length=>"+msgList.length);
			if(msgList.length == 0) {
				mui.toast("没有历史消息了");
				return;
			}
			var msgSeq = msgList[0].seq - 1;
			getPrePageGroupHistroyMsgInfoMap[selToID] = {
				"ReqMsgSeq": msgSeq
			};

			if(cbOk) {
				cbOk(msgList);
			} else {
				getHistoryMsgCallback(selToID, msgList, true);
			}
		},
		function(err) {
			console.log(err.ErrorInfo);
		}
	);
};