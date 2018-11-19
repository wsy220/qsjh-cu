(function(w) {
	document.addEventListener('plusready', function() {
		console.log("QS_Immersed-UserAgent: " + navigator.userAgent);
	}, false);

	var immersed = 0;
	var ms = (/Html5Plus\/.+\s\(.*(Immersed\/(\d+\.?\d*).*)\)/gi).exec(navigator.userAgent);
	if(ms && ms.length >= 3) {
		immersed = parseFloat(ms[2]);
	}
	w.immersed = immersed;

	if(!immersed) {
		return;
	}
	var topoffset = '45px';
	// 获取状态栏高度并根据业务需求处理，这里重新计算了子窗口的偏移位置
	topoffset = (immersed + 45) + 'px';
	document.querySelector("header").style.height = topoffset;
	document.querySelector("header").style.paddingTop = immersed + "px";
	//document.getElementById('content').style.paddingTop = topoffset;
	document.querySelector(".mui-bar-nav~.mui-content").style.paddingTop = topoffset;
	if(document.getElementById('cus-modal')) {
		document.getElementById('cus-modal').style.top = topoffset;
	}
	if(document.getElementById('cus-wrapper')) {
		document.getElementById('cus-wrapper').style.top = topoffset;
	}
})(window);