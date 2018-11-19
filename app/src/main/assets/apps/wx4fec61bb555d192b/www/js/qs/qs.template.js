;
(function($, window, document, undefined) {

	if(!window.tempFunction) {
		window.tempFunction = {};
	}
	//定义Beautifier的构造函数
	var AjaxElement = function() {}
	//定义Beautifier的方法
	AjaxElement.prototype = {
		getajaxHttp: function() {
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
		},
		ajaxrequest: function(url, methodtype, con, parameter, functionName, obj) {
			var xmlhttp = this.getajaxHttp();
			xmlhttp.onreadystatechange = function() {
				if(xmlhttp.readyState == 4) {
					//HTTP响应已经完全接收才调用
					functionName(xmlhttp, obj);
				}
			};
			xmlhttp.open(methodtype, url, con);
			xmlhttp.send(parameter);
		},

		processTLF: function(obj, func) {
			if(tempFunction[obj.id]) {
				func(tempFunction[obj.id])
			} else {
				this.ajaxrequest(obj.src, 'get', true, null, function(http, obj) {
					//预编译模版
					var temp = template.compile(http.responseText.replace(/^\s*|\s*$/g, ""));
					tempFunction[obj.id] = temp
					func(tempFunction[obj.id])
				}, obj);
			}
		}
	}

	$.fn.processTL = function() {
		//		for(var k in arguments) {
		//			console.log(`#${k} type: ${typeof(arguments[k])} value: ${arguments[k]}`);
		//		}
		var element = this;
		var obj = arguments[0];
		var data = arguments[1];
		var method = arguments[2];
		var endMethod = arguments[3];
		if(!method) {
			method = 'append';
		}
		if(!obj) {
			throw "请检查模版配置";
		}
		var ajaxElement = new AjaxElement();
		ajaxElement.processTLF(obj, function(func) {
			element[method](func(data))
			if(endMethod){
				endMethod();
			}
		});
	},

	$.processTLE = function() {
		var obj = arguments[0];
		var data = arguments[1];
		var method = arguments[2];
		if(!obj) {
			throw "请检查模版配置";
		}
		var ajaxElement = new AjaxElement();
		ajaxElement.processTLF(obj, function(func) {
			method(func(data))
		});
	},
	
	$.getLength = function (str) {
        ///<summary>获得字符串实际长度，中文2，英文1</summary>
        ///<param name="str">要获得长度的字符串</param>
        var realLength = 0, len = str.length, charCode = -1;
        for (var i = 0; i < len; i++) {
            charCode = str.charCodeAt(i);
            if (charCode >= 0 && charCode <= 128) realLength += 1;
            else realLength += 2;
        }
        return realLength;
   },

    //js截取字符串，中英文都能用  
    //如果给定的字符串大于指定长度，截取指定长度返回，否者返回源字符串。  
    //字符串，长度  

    /** 
     * js截取字符串，中英文都能用 
     * @param str：需要截取的字符串 
     * @param len: 需要截取的长度 
     */
    $.cutstr = function (str, len) {
        var str_length = 0;
        var str_len = 0;
        str_cut = new String();
        str_len = str.length;
        for (var i = 0; i < str_len; i++) {
            a = str.charAt(i);
            str_length++;
            if (escape(a).length > 4) {
                //中文字符的长度经编码之后大于4  
                str_length++;
            }
            str_cut = str_cut.concat(a);
            if (str_length >= len) {
                str_cut = str_cut.concat("...");
                return str_cut;
            }
        }
        //如果给定字符串小于指定长度，则返回源字符串；  
        if (str_length < len) {
            return str;
        }
    }
    
})(jQuery, window, document);