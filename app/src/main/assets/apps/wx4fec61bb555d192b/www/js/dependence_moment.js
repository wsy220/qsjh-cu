var gTime = (function() {
	var self = this;

	/*
	 *    功能:  获取当前时间  默认格式为  2016-6-6 14:27:00
	 *    方法名: fGetCurrentTime
	 * 	      所属类: gTime
	 *    参数:  sFormat 格式化字符串
	 * 	     返回值: 当前时间  默认格式为 2016-6-6 14:27:00
	 */
	self.fGetCurrentTime = function(sFormat) {
		var sFM = sFormat || 'YYYY-MM-DD hh:mm:ss';
		return moment().format(sFM);
	}
	/*
	 *    功能:  获取当前年份
	 *    方法名: fGetCurrentTime
	 * 	      所属类: gTime
	 *    参数:  
	 * 	     返回值: 当前年份
	 */
	self.fGetCurrentYear = function() {
		return moment().format('YYYY');
	}
	/*
	 *    功能:  获取当前月份
	 *    方法名: fGetCurrentMonth
	 * 	      所属类: gTime
	 *    参数:  
	 * 	     返回值: 当前月份
	 */
	self.fGetCurrentMonth = function() {
		return moment().format('MM');
	}
	/*
	 *    功能:  获取今天几号
	 *    方法名: fGetCurrentDay
	 * 	      所属类: gTime
	 *    参数:  
	 * 	     返回值: 今天几号
	 */
	self.fGetCurrentDay = function() {
		return moment().format('DD');
	}
	/*
	 *    功能:  获取今天时
	 *    方法名: fGetCurrentHour
	 * 	      所属类: gTime
	 *    参数:  
	 * 	     返回值: 今天时
	 */
	self.fGetCurrentHour = function() {
		return moment().format('hh');
	}
	/*
	 *    功能:  获取当前几分
	 *    方法名: fGetCurrentMinute
	 * 	      所属类: gTime
	 *    参数:  
	 * 	     返回值: 获取当前几分
	 */
	self.fGetCurrentMinute = function() {
		return moment().format('mm');
	}
	/*
	 *    功能:  当前星期几
	 *    方法名: fGetCurrentWeek
	 * 	      所属类: gTime
	 *    参数:  nDays 几天前
	 * 	     返回值: 几天前日期
	 */
	self.fGetCurrentWeek = function(end) {
		var sWeek = moment(end).format('dddd');
		switch(sWeek) {
			case 'Monday':
				sWeek = '星期一';
				break;
			case 'Tuesday':
				sWeek = '星期二';
				break;
			case 'Wednesday':
				sWeek = '星期三';
				break;
			case 'Thursday':
				sWeek = '星期四';
				break;
			case 'Friday':
				sWeek = '星期五';
				break;
			case 'Saturday':
				sWeek = '星期六';
				break;
			case 'Sunday':
				sWeek = '星期日';
				break;
			default:
				break;
		}
		return sWeek;
	}
	/*
	 *    功能:  获取X天前日期
	 *    方法名: fGetBefareDay
	 * 	      所属类: gTime
	 *    参数:  nDays 几天前
	 * 	     返回值: 几天前日期
	 */
	self.fGetBefareDay = function(nDays) {
		var sBDay = moment().subtract(nDays, 'days');
		return moment(sBDay).format('YYYY-MM-DD');
	}
	/*
	 *    功能:  获取X天后日期
	 *    方法名: fGetAfterDay
	 * 	      所属类: gTime
	 *    参数:  nDays 几天后
	 * 	     返回值: 几天后日期
	 */
	self.fGetAfterDay = function(nDays) {
		var sADay = moment().add(nDays, 'days');
		return moment(sADay).format('YYYY-MM-DD');
	}
	self.initSearchMajorChanges = function(nDays){
        //实现当前日期是当年的第几周,再向前和向后推几周,js数组保存当前日期的前后两周(共五周的数据)
        var vv=nDays;
        var vNowDate=moment(new moment(vv).format("YYYY-MM-DD"));//.add('month',0).add('days',-1);
        var vWeekOfDay=moment(vNowDate).format("E");//算出这周的周几
        var vWeekOfDays=7-vWeekOfDay-1;
        var vStartDate=moment(vNowDate).add('days',vWeekOfDays);
        var vEndDate=moment(vNowDate).add('days',-vWeekOfDay);
        var vStartDateNew=moment(vStartDate).add('days',7*$scope.gWeeks);
        var vEndDateNew=moment(vEndDate).add('days',-(7*$scope.gWeeks));
        //var vYearAndWeek=moment(vStartDate).format("YYYY")+'-'+moment(vStartDate).format("WW");
        searchMajorChanges(vStartDateNew,vEndDateNew);
    }
	
	return this;
})();