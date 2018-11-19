function sdkApi(req, data) {
	var cmdName = req.cmdName;
	var adminSign = req.adminSign;
	var serviceName = req.serviceName;
	var adminIdentifier = "admin";
	var sdkappid = "1400053640";
	$.ajax({
		url: 'https://console.tim.qq.com/v4/' + serviceName + '/' + cmdName + '?usersig=' + adminSign + '&apn=1&identifier=' + adminIdentifier + '&sdkappid=' + sdkappid + '&contenttype=json',
		data: JSON.stringify(data),
		type: 'POST',
		success: function(data) {
			var ret = jQuery.parseJSON(data);
			console.log('sendMsg**Success' + JSON.stringify(ret))
			if(ret.ErrorCode=="10010"){
				mui.toast('订单已完结，消息发送失败');
			}
		},
		error: function(xhr, status, error) {
			console.log('sendMsg**Error: ' + error.message);
		},
	});
}

function sendMsg(to_account, msgbody, selType) {
	console.log("sendMsg---selType->"+selType);
	console.log("sendMsg---to_account->"+to_account);
	if(selType == "GROUP") {
		var apireq = {};
		apireq.cmdName = "send_group_msg";
		apireq.adminSign = localStorage.getItem("adminSig");
		apireq.serviceName = "group_open_http_svc";
		var from_account = localStorage.getItem("userID");
		var msgRandom = parseInt(999999 * Math.random());
		var data = {
			GroupId: to_account,
			From_Account: from_account,
			MsgRandom: msgRandom,
			MsgBody: [
				msgbody
			]
		}
		sdkApi(apireq, data);
	} else {
		var apireq = {};
		apireq.cmdName = "sendmsg";
		apireq.adminSign = localStorage.getItem("adminSig");
		apireq.serviceName = "openim";
		var from_account = localStorage.getItem("userID");
		var msgRandom = parseInt(999999 * Math.random());
		var data = {
			SyncOtherMachine: 1,
			To_Account: to_account,
			From_Account: from_account,
			MsgRandom: msgRandom,
			MsgBody: [
				msgbody
			]
		}
		sdkApi(apireq, data);
	}

}
//  function tryapi() {
//    var msgbody = {
//      "MsgType": "TIMTextElem",
//      "MsgContent": {
//        "Text": "hello world"
//      }};
//    to_account = "xiaoming";
//    sendMsg(to_account,msgbody);
//  }