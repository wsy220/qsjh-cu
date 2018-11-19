/*响应式字体*/
(function(doc, win) {
	var docEl = doc.documentElement,
		resizeEvt = 'orientationchange' in window ? 'orientationchange' : 'resize',
		recalc = function() {
			var clientWidth = docEl.clientWidth;
			if(!clientWidth) return;
			if(clientWidth >= 720) {
				docEl.style.fontSize = '100px';
			} else {
				docEl.style.fontSize = 100 * (clientWidth / 720) + 'px';
			}
		};

	if(!doc.addEventListener) return;
	win.addEventListener(resizeEvt, recalc, false);
	doc.addEventListener('DOMContentLoaded', recalc, false);
})(document, window);

// 预留另一种响应式写法
//(function(win) {
//function resize() {
//    var domWidth = domEle.getBoundingClientRect().width;
//    if(domWidth / v > 750){
//        domWidth = 750 * v;
//    }
//    win.rem = domWidth / 16;
//    domEle.style.fontSize = win.rem + "px";
//}
//var v, initial_scale, timeCode, dom = win.document, domEle = dom.documentElement, viewport = dom.querySelector('meta[name="viewport"]'), flexible = dom.querySelector('meta[name="flexible"]');
//if (viewport) {
//    var o = viewport.getAttribute("content").match(/initial\-scale=(["']?)([\d\.]+)\1?/);
//    if(o){
//        initial_scale = parseFloat(o[2]);
//        v = parseInt(1 / initial_scale);
//    }
//} else if(flexible) {
//    var o = flexible.getAttribute("content").match(/initial\-dpr=(["']?)([\d\.]+)\1?/);
//    if (o) {
//        v = parseFloat(o[2]);
//        initial_scale = parseFloat((1 / v).toFixed(2))
//    }
//}
//if (!v && !initial_scale) {
//    var n = (win.navigator.appVersion.match(/android/gi), win.navigator.appVersion.match(/iphone/gi));
//    v = win.devicePixelRatio;
//    v = n ? v >= 3 ? 3 : v >= 2 ? 2 : 1 : 1, initial_scale = 1 / v
//}
////没有viewport标签的情况下
//if (domEle.setAttribute("data-dpr", v), !viewport) {
//    if (viewport = dom.createElement("meta"), viewport.setAttribute("name", "viewport"), viewport.setAttribute("content", "initial-scale=" + initial_scale + ", maximum-scale=" + initial_scale + ", minimum-scale=" + initial_scale + ", user-scalable=no"), domEle.firstElementChild) {
//        domEle.firstElementChild.appendChild(viewport)
//    } else {
//        var m = dom.createElement("div");
//        m.appendChild(viewport), dom.write(m.innerHTML)
//    }
//}
//win.dpr = v;
//win.addEventListener("resize", function() {
//    clearTimeout(timeCode), timeCode = setTimeout(resize, 300)
//}, false);
//win.addEventListener("pageshow", function(b) {
//    b.persisted && (clearTimeout(timeCode), timeCode = setTimeout(resize, 300))
//}, false);
//resize();
//})(window);