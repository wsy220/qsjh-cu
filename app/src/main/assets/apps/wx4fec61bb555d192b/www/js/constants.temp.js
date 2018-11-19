var tempFunction = {};
/**
 * 得到ajax对象
 */
function getajaxHttp() {
	var xmlHttp;
	try {
		// Firefox, Opera 8.0+, Safari
		xmlHttp = new XMLHttpRequest();
	} catch(e) {
		// Internet Explorer
		try {
			xmlHttp = new ActiveXObject("Msxml2.XMLHTTP");
		} catch(e) {
			try {
				xmlHttp = new ActiveXObject("Microsoft.XMLHTTP");
			} catch(e) {
				alert("您的浏览器不支持AJAX！");
				return false;
			}
		}
	}
	return xmlHttp;
}
/**
 * 发送ajax请求
 * url--url
 * methodtype(post/get)
 * con (true(异步)|false(同步))
 * parameter(参数)
 * functionName(回调方法名，不需要引号,这里只有成功的时候才调用)
 * (注意：这方法有二个参数，一个就是xmlhttp,一个就是要处理的对象)
 * obj需要到回调方法中处理的对象
 */
function ajaxrequest(url, methodtype, con, parameter, functionName, obj) {
	var xmlhttp = getajaxHttp();
	xmlhttp.onreadystatechange = function() {
		if(xmlhttp.readyState == 4) {
			//HTTP响应已经完全接收才调用
			functionName(xmlhttp, obj);
		}
	};
	xmlhttp.open(methodtype, url, con);
	xmlhttp.send(parameter);
}

function processTL(id, func) {
	if(tempFunction[id]) {
		func(tempFunction[id])
	} else {
		var t = document.getElementById(id);
		if(!t.id || !t.id.length || !t.src || !t.src.length || 'text/html' != t.type)
			throw '标签错误';
		var obj = {
			src: t.src,
			id: t.id
		};
		//清除原来的模板信息
		t.id = "";
		t.src = "";
		ajaxrequest(obj.src, 'get', true, null, function(http, obj) {
			//预编译模版
			var temp = template.compile(http.responseText.replace(/^\s*|\s*$/g, ""));
			tempFunction[obj.id] = temp
			func(tempFunction[obj.id])
		}, obj);
	}
}

//(function() {
//	var tems = document.getElementsByTagName('script');
//
//	for(var i = 0; i < tems.length; i++) {
//		var t = tems[i];
//		if(!t.id || !t.id.length || !t.src || !t.src.length || 'text/html' != t.type) continue;
//		var obj = {
//			src: t.src,
//			id: t.id
//		};
//		//清除原来的模板信息
//		t.id = "";
//		t.src = "";
//		ajaxrequest(obj.src, 'get', true, null, function(http, obj) {
//			//预编译模版
//			var temp = template.compile(http.responseText.replace(/^\s*|\s*$/g, ""));
//			tempFunction[obj.id] = temp
//		}, obj);
//	}
//})();

template.defaults.imports.formatString = function(typestr, type) {

	if(type == 'notification') {
		if(typestr == 'order') {
			return '../images/ddxx.png'
		} else {
			return '../images/xtxx.png'
		}
	} else if(type == 'datestr') {
		var year = typestr.substring(0, 10);
		var day = typestr.substring(11, 19);
		//year1 = year1.replaceAll("-", "/")
		return year + ' ' + day;
	} else {
		return typestr;
	}

}