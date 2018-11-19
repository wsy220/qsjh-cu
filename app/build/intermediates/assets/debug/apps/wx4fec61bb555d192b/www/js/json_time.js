/*json日期格式转换--YYYY-MM-DD HH24:MI:SS*/
function date_All_format() {
	var date = new Date();
	var month = date.getMonth() + 1 < 10 ? "0" + (date.getMonth() + 1) : date.getMonth() + 1;
	var day = date.getDate() < 10 ? "0" + date.getDate() : date.getDate();
	var hours = date.getHours() < 10 ? "0" + date.getHours() : date.getHours();
	var minutes = date.getMinutes() < 10 ? "0" + date.getMinutes() : date.getMinutes();
	var seconds = date.getSeconds() < 10 ? "0" + date.getSeconds() : date.getSeconds();
	var date_sysdate = date.getFullYear() + '-' + month + '-' + day + " " + hours + ":" + minutes + ":" + seconds;
	return date_sysdate;
}

/*json日期格式转换--YYYY-MM-DD*/
function date_Day_format(json_time) {
	var date = new Date(JSON.parse(json_time).time);
	var month = date.getMonth() + 1 < 10 ? "0" + (date.getMonth() + 1) : date.getMonth() + 1;
	var day = date.getDate() < 10 ? "0" + date.getDate() : date.getDate();
	var date_sysdate = date.getFullYear() + '-' + month + '-' + day;
	return date_sysdate;
}

/*计算2个时间相差几分钟和秒数*/
function date_Time_minus(json_time, json_time2) {
	var date1 = new Date(json_time.replace(/-/g, '/')); //日期中的'-'全部替换为'/'用来计算毫秒数
	var date2 = new Date(json_time2.replace(/-/g, '/')); //日期中的'-'全部替换为'/'用来计算毫秒数
	var total = (date2.getTime() - date1.getTime()) / 1000;
	var day = parseInt(total / (24 * 60 * 60)); //计算整数天数
	var afterDay = total - day * 24 * 60 * 60; //取得算出天数后剩余的秒数
	var hour = parseInt(afterDay / (60 * 60)); //计算整数小时数
	var afterHour = total - day * 24 * 60 * 60 - hour * 60 * 60; //取得算出小时数后剩余的秒数
	var min = parseInt(afterHour / 60); //计算整数分
	/*var minutes = min < 10 ? "0" + min : min; // 分数不足2位用0补齐*/
	var minutes = hour * 60 + min;
	minutes = minutes < 10 ? "0" + minutes : minutes
	var afterMin = total - day * 24 * 60 * 60 - hour * 60 * 60 - min * 60; //取得算出分后剩余的秒数
	var seconds = afterMin < 10 ? "0" + afterMin : afterMin; // 秒数不足2位用0补齐
	var return_time = minutes + ':' + seconds;
	return return_time;
}

/*json日期格式转换--YYYY-MM-DD HH24:MI*/
function date_Minutes_format(json_time) {
	var date = new Date(JSON.parse(json_time).time);
	var month = date.getMonth() + 1 < 10 ? "0" + (date.getMonth() + 1) : date.getMonth() + 1;
	var day = date.getDate() < 10 ? "0" + date.getDate() : date.getDate();
	var hours = date.getHours() < 10 ? "0" + date.getHours() : date.getHours();
	var minutes = date.getMinutes() < 10 ? "0" + date.getMinutes() : date.getMinutes();
	var date_sysdate = date.getFullYear() + '-' + month + '-' + day + " " + hours + ":" + minutes;
	return date_sysdate;
}

function getDate(strDate) {
	//	var date = eval('new Date(' + strDate.replace(/\d+(?=-[^-]+$)/,
	//		function(a) {
	//			return parseInt(a, 10) - 1;
	//		}).match(/\d+/g) + ')');
	var date = moment(strDate).format('YYYY-MM-DD HH:mm:ss');
	return date;
}

function getDate_formate(strDate) {
	var date = moment(strDate).format('YYYY-MM-DD');
	return date;
}

function StringToDate(DateStr) {
	var converted = Date.parse(DateStr);
	var myDate = new Date(converted);
	if(isNaN(myDate)) {
		//var delimCahar = DateStr.indexOf('/')!=-1?'/':'-';  
		var arys = DateStr.split('-');
		myDate = new Date(arys[0], --arys[1], arys[2]);
	}
	return myDate;
}

function CurentTime() {
	var now = new Date();
	var year = now.getFullYear(); //年
	var month = now.getMonth() + 1; //月
	var day = now.getDate(); //日
	var hh = now.getHours(); //时
	var mm = now.getMinutes(); //分
	var clock = year + "-";

	if(month < 10)
		clock += "0";

	clock += month + "-";

	if(day < 10)
		clock += "0";

	clock += day + " ";

	//	if(hh < 10)
	//		clock += "0";
	//
	//	clock += hh + ":";
	//	if(mm < 10) clock += '0';
	//	clock += mm;
	return(clock);
}
//获取几天后的日期
function GetDateStr(AddDayCount) {
	var dd = new Date();
	dd.setDate(dd.getDate() + AddDayCount); //获取AddDayCount天后的日期 
	var y = dd.getFullYear();
	var m = dd.getMonth() + 1; //获取当前月份的日期 
	var d = dd.getDate();
	return y + "-" + m + "-" + d;
}

function GetDayStr(AddDayCount) {
	var dd = new Date();
	dd.setDate(dd.getDate() + AddDayCount); //获取AddDayCount天后的日期 
	var y = dd.getFullYear();
	var m = dd.getMonth() + 1; //获取当前月份的日期 
	var d = dd.getDate();
	return d;
}

function GetMonthStr(AddDayCount) {
	var dd = new Date();
	dd.setDate(dd.getDate() + AddDayCount); //获取AddDayCount天后的日期 
	var y = dd.getFullYear();
	var m = dd.getMonth() + 1; //获取当前月份的日期 
	var d = dd.getDate();
	return m;
}

function leftTimer(year, month, day, hour, minute, second) {
	var leftTime = (new Date(year, month - 1, day, hour, minute, second)) - (new Date()); //计算剩余的毫秒数 
	var days = parseInt(leftTime / 1000 / 60 / 60 / 24, 10); //计算剩余的天数 
	var hours = parseInt(leftTime / 1000 / 60 / 60 % 24, 10); //计算剩余的小时 
	var minutes = parseInt(leftTime / 1000 / 60 % 60, 10); //计算剩余的分钟 
	var seconds = parseInt(leftTime / 1000 % 60, 10); //计算剩余的秒数 
	days = checkTime(days);
	hours = checkTime(hours);
	minutes = checkTime(minutes);
	seconds = checkTime(seconds);
}

function checkTime(i) { //将0-9的数字前面加上0，例1变为01 
	if(i < 10) {
		i = "0" + i;
	}
	return i;
}