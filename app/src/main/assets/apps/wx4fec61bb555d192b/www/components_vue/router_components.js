const yuyueguahao = {
	template: '',
	created() {
		mui.openWindow({
			id: 'tl_bank_pay.html',
			url: '../document/tl_bank_pay.html',
			show: {
				autoShow: false, //页面loaded事件发生后自动显示，默认为true
				event: 'loaded' //页面显示时机，默认为titleUpdate事件时显示
			},
			waiting: {
				autoShow: false //自动显示等待框，默认为true
			}
		});
	}
}
const baogaojiedu = {
	template: '',
	created() {
		mui.openWindow({
			url: "../online_answer_order/online_answer_index.html",
			id: "online_answer_index.html",
			styles: {
				//titleNView: titleNView
			}
		});
	}
}
const tijianbaogao = {
	template: '',
	created() {
		if(localStorage.getItem("TOKENID") == null) {
			mui.openWindow({
				url: "../login/login.html",
				id: 'login/login.html'
			});
			return false;
		}

		mui.openWindow({
			url: "bodyCheck_report_withVue.html",
			id: "bodyCheck_report_withVue.html",
			styles: {
				//titleNView: titleNView
			}
		});
	}
}