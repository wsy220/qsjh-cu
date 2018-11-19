/*	时间选择器封装	@山东-三言
 *  共有两种初始化方式：
 * 	@第一种方式：mui(".mui-getDate").getDateInfo();		类名可以随便定义,优点可以定义多项，缺点无法设置回调函数
 * 	@第二种方式：var test = new mui.getDateInfo(document.getElementById("testGetDate"),function(d){alert(d)});	
 * 		@第一个参数为element类型，就是要初始化的节点
 * 		@第二个参数为function类型，回调函数，回调的有一个参数为获取的时间字符串
 * ------------------------------------------------------------
 *  时间类型说明				例子
 *	data命名	data-dateType	<input class="mui-getDate" data-dateType="date" id='' name=''>
 * 				
 * 	可选值	返回结果说明
 *  date	日期类型，例如 2016-11-11
 *  time	时间类型，例如 18:22		24小时制
 *  @! 如果不设置data-dateType默认为date类型
 * ------------------------------------------------------------
 * 【data-dateType为date类型】
 * data参数名				说明	     	默认值	格式都为"2016-11-11"	
 * data-defaultDate		默认选中的时间	当前日期
 * data-minDate			日期最小值		无
 * data-maxDate			日期最大值		无
 * 
 * 【data-dateType为time类型】
 * data参数名				说明	     			默认值	格式都为"13:22"	
 * data-defaultDate		data-defaultDate	12:00	
 * -----------------------------------------------------------
 * 返回值	如果有回调函数会调用回调函数，返回值为选中的日期字符串
 * 		当element为input的时候会设置value为选中的日期字符串
 */
(function($){
	var GetDateInfo = $.getDateInfo = $.Class.extend({
		init: function(element, fun) {
			var self = this;
			if(element){
				if(element.nodeType) {
					self.ele = element;
					fun && (self.fun = fun);
					self.EventInit();
				} else {
					throw new Error('传入的参数不是Element类型！');
				}
				return self;
			}
		},
		//绑定事件
		EventInit: function() {
			var self = this,
				selfEle = this.ele,
				isBindingEvent = selfEle.hasAttribute("data-getDate-event"); //是否绑定点击事件
			!isBindingEvent && selfEle.addEventListener("tap", function() {
				self.getDate()
			}), selfEle.setAttribute("data-getDate-event", true); //绑定点击事件
			!selfEle.hasAttribute("readonly") && selfEle.tagName == 'INPUT' && selfEle.setAttribute("readonly", "readonly"); //如果是INPUT添加禁用状态
			return self;
		},

		/*为了不犯和官方numbox插件不能动态设置最大值和最小值的错误，本插件采用的都是实时获取attr的值*/
		getDate: function() {
			var self = this,
				selfEle = self.ele,
				attrDateType = selfEle.getAttribute("data-dateType") || "date";
				attrDateType == "date" && self.setDate();
				attrDateType == "time" && self.setTime();
			return self;
		},
		setDate: function() {
			var self = this,
				selfEle = self.ele,
				attrDefaultDate = selfEle.getAttribute("data-defaultDate"), //获取attr设置的当前日期
				dDate = new Date(), //当前日期对象
				activeDate = [dDate.getFullYear(), dDate.getMonth(), dDate.getDate()], //获取当前日期
				attrMinDate = selfEle.getAttribute("data-minDate"), //获取最小日期
				attrMaxDate = selfEle.getAttribute("data-maxDate"), //获取最大日期
				minDate, maxDate; //最大值、最大值对象

			/*设置默认日期*/
			attrDefaultDate ? dDate = self.setDateStr(attrDefaultDate) : dDate.setFullYear(activeDate[0], activeDate[1], activeDate[2]);
			/*设置最小日期*/
			attrMinDate && (minDate = self.setDateStr(attrMinDate));
			/*设置最大日期*/
			attrMaxDate && (maxDate = self.setDateStr(attrMaxDate));

			plus.nativeUI.pickDate(function(e) {
				var d = e.date,
					dStr = d.getFullYear() + "-" + (d.getMonth() + 1) + "-" + d.getDate();
				
				self.fun && self.fun(dStr,selfEle);
				selfEle.tagName == 'INPUT' && (selfEle.value = dStr);
				
			}, function(e) {
				
			}, {
				title: "请选择日期",
				date: dDate,
				minDate: minDate,
				maxDate: maxDate
			});
		},
		setTime: function() {
			var self = this,
				selfEle = self.ele,
				attrDefaultTime = selfEle.getAttribute("data-defaultTime")?selfEle.getAttribute("data-defaultTime").split(":"):null,
				dTime = new Date();
			
			attrDefaultTime?dTime.setHours(attrDefaultTime[0], attrDefaultTime[1]):dTime.setHours(12, 0);
			plus.nativeUI.pickTime(function(e) {
				var d = e.date,
					dStr = d.getHours() + ":" + d.getMinutes();
				
				selfEle.tagName == 'INPUT' && (selfEle.value = dStr);
				self.fun && self.fun(dStr,selfEle);
			}, function(e) {
				
			}, {
				title: "请选择时间",
				is24Hour: true,
				time: dTime
			});
			return self;
		},
		
		setDateStr: function(str) {
			if(typeof str === 'string') {
				var returnAry = str.split("-").map(function(item, index) {
					if(index == 1) {
						return +item - 1;
					} else {
						return +item;
					}
				});
				var dateObj = new Date();
				dateObj.setFullYear(returnAry[0], returnAry[1], returnAry[2]);
				return dateObj;
			} else {
				throw new Error('传入的参数不是字符串类型！');
			}
		}
	});

	$.fn.getDateInfo = function() {
		this.each(function(i, item) {
			if(item.GetDateInfo) {
				return false;
			} else {
				item.getDateInfo = new GetDateInfo(item);
				return true;
			}
		});
	};
})(mui);