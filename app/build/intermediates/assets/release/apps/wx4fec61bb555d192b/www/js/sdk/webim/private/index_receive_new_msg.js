//监听新消息事件
var msgList = [];
var dateStart = null;
var dateEnd = null;


//newMsgList 为新消息数组，结构为[Msg]
function onMsgNotify(newMsgList) {
	plus.webview.getWebviewById("subpages/tab-webview-subpage-talk.html").evalJS("changeRed()");
	localStorage.setItem('showRed', 'true');
	console.log("****有新消息");
	//创建消息提示
	var data = "{\"dc\":\"msg001\"}";
	plus.push.createMessage( "新消息", data, null );
}