;
(function($, window, document, undefined) {

	$.initIscroll = function(oprtion) {
		var $scrollDiv = $("#" + oprtion.id);
		var myScroll;
		var pullDownEl /*, pullDownL*/ ;
		var pullUpEl /*, pullUpL*/ ;

		$scrollDiv.find(".scroller").prepend('<div id="pullDown" class="pullDown ub ub-pc c-gra"><div class="help"></div></div>')
		$scrollDiv.find(".scroller").append('<div id="pullUp" class="pullUp ub ub-pc c-gra"><div class="typing_loader"></div></div>')

		pullDownEl = $scrollDiv.find('.pullDown');
		//pullDownL = pullDownEl.find('.pullDownLabel');
		pullDownEl['class'] = pullDownEl.attr('class');
		pullDownEl.attr('class', '').hide();

		pullUpEl = $scrollDiv.find('.pullUp');
		//pullUpL = pullUpEl.find('.pullUpLabel');
		pullUpEl['class'] = pullUpEl.attr('class');
		pullUpEl.attr('class', '').hide();

		myScroll = new IScroll('#' + oprtion.id, {
			probeType: 2, //probeType：1对性能没有影响。在滚动事件被触发时，滚动轴是不是忙着做它的东西。probeType：2总执行滚动，除了势头，反弹过程中的事件。这类似于原生的onscroll事件。probeType：3发出的滚动事件与到的像素精度。注意，滚动被迫requestAnimationFrame（即：useTransition：假）。  
			scrollbars: false, //有滚动条  
			mouseWheel: true, //允许滑轮滚动  
			fadeScrollbars: true, //滚动时显示滚动条，默认影藏，并且是淡出淡入效果  
			bounce: true, //边界反弹  
			interactiveScrollbars: true, //滚动条可以拖动  
			shrinkScrollbars: 'scale', // 当滚动边界之外的滚动条是由少量的收缩。'clip' or 'scale'.  
			click: true, // 允许点击事件  
			keyBindings: true, //允许使用按键控制  
			momentum: true // 允许有惯性滑动  
		});

		myScroll.cusId = oprtion.id;
		myScroll.pullDownEl = pullDownEl;
		//myScroll.pullDownL = pullDownL;
		myScroll.pullUpEl = pullUpEl;
		//myScroll.pullUpL = pullUpL;
		myScroll.loadingStep = 0; //加载状态0默认，1显示加载状态，2执行加载数据，只有当为0时才能再次加载，这是防止过快拉动刷新  
		myScroll.downFlag = true;
		myScroll.upFlag = true;

		myScroll.page = oprtion.page | 1;
		myScroll.limit = oprtion.limit | 5
		myScroll.bussType = oprtion.bussType;

		//滚动时
		myScroll.on('scroll', function() {
			if(myScroll.loadingStep == 0 && !pullDownEl.class.match('flip|loading') && !pullUpEl.class.match('flip|loading')) {
				if(this.y > 20) {
					myScroll.page = 1;
					//下拉刷新效果  
					pullDownEl.attr('class', pullDownEl['class'])
					pullDownEl.show();
					myScroll.refresh();
					pullDownEl.addClass('flip');
					//pullDownL.html('准备刷新...');
					myScroll.loadingStep = 1;
				} else if(this.y < (this.maxScrollY - 5) && myScroll.upFlag) {
					//上拉刷新效果  
					pullUpEl.attr('class', pullUpEl['class'])
					pullUpEl.show();
					myScroll.refresh();
					pullUpEl.addClass('flip');
					//pullUpL.html('准备刷新...');
					myScroll.loadingStep = 1;
				}
			}
		});
		//滚动完毕  
		myScroll.on('scrollEnd', function() {
			if(myScroll.loadingStep == 1) {
				if(pullUpEl.attr('class').match('flip|loading')) {
					pullUpEl.removeClass('flip').addClass('loading');
					//pullUpL.html('拼命加载中 ...');
					myScroll.loadingStep = 2;
					oprtion.pullUpAction(myScroll);
					setTimeout(function() {
						resetPullUp(myScroll)
					}, 1000); //1秒  
				} else if(pullDownEl.attr('class').match('flip|loading')) {
					pullDownEl.removeClass('flip').addClass('loading');
					//pullDownL.html('拼命加载中 ...');
					myScroll.loadingStep = 2;
					oprtion.pullDownAction(myScroll);
					setTimeout(function() {
						resetPullDown(myScroll);
					}, 1000); //1秒  
				}
			}
		});

		resetPullDown = function(iScroll) {
			iScroll.pullDownEl.removeClass('loading');
			//iScroll.pullDownL.html('下拉显示更多...');
			iScroll.pullDownEl['class'] = iScroll.pullDownEl.attr('class');
			iScroll.pullDownEl.attr('class', '').hide();
			iScroll.refresh();
			iScroll.loadingStep = 0;
		}
		resetPullUp = function(iScroll) {
			iScroll.pullUpEl.removeClass('loading');
			//iScroll.pullUpL.html('上拉显示更多...');
			iScroll.pullUpEl['class'] = iScroll.pullUpEl.attr('class');
			iScroll.pullUpEl.attr('class', '').hide();
			iScroll.refresh();
			iScroll.loadingStep = 0;
		}

		return myScroll;
	}

	$.resetPullDown = function(iScroll) {
		iScroll.pullDownEl.removeClass('loading');
		//iScroll.pullDownL.html('下拉显示更多...');
		iScroll.pullDownEl['class'] = iScroll.pullDownEl.attr('class');
		iScroll.pullDownEl.attr('class', '').hide();
		iScroll.refresh();
		iScroll.loadingStep = 0;
	}

	$.resetPullUp = function(iScroll) {
		iScroll.pullUpEl.removeClass('loading');
		//iScroll.pullUpL.html('上拉显示更多...');
		iScroll.pullUpEl['class'] = iScroll.pullUpEl.attr('class');
		iScroll.pullUpEl.attr('class', '').hide();
		iScroll.refresh();
		iScroll.loadingStep = 0;
	}

	$.getPageUrl = function(url, iScroll) {
		var rurl = url
		return rurl;
	}

})(jQuery, window, document);