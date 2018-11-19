/*项目详情组件*/
var headerTemplate_img = '<div class="service_item_pc"><div class="mui-content-padded"style="text-align: center; padding-top:5px">平台优势</div><ul class="mui-table-view mui-grid-view mui-grid-9"style="background-color: #FFFFFF;"><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">职业服务&nbsp;专业服务</div></a></li><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">医患投保&nbsp;全程安心</div></a></li><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">专业团队&nbsp;随时售后</div></a></li></ul><ul class="mui-table-view firstUl"id="table-viewX_1"><li class="mui-table-view-cell mui-media"><a class="mui-navigate-right"id="wodeyihu"><div class="tubiao"><img class="mui-media-object mui-pull-left"src="../images/wode/wdyh.png"></div><div class="mui-media-body"style="padding-top: 0.25rem;" v-on:click="greet(post.value)">{{post.label}}</div></a></li></ul><ul class="mui-table-view"id="table-viewX_2"><li class="mui-table-view-cell"><a class="mui-navigate-right"id="service_object"><div class="tubiao"><img class="mui-media-object mui-pull-left"src="../images/wode/ffdx.png"/></div><div class="mui-media-body"style="padding-top: 0.25rem;">{{post.label}}</div></a></li></ul></div>'
var header1 = '<div class="service_item_pc"><div class="mui-content-padded"style="text-align: center;">平台优势</div><ul class="mui-table-view mui-grid-view mui-grid-9"style="background-color: #FFFFFF;"><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">职业服务&nbsp;专业服务</div></a></li><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">医患投保&nbsp;全程安心</div></a></li><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">专业团队&nbsp;随时售后</div></a></li></ul></div>'
var header2 = '<div class="service_item_pc"><ul class="mui-table-view firstUl"id="table-viewX_1"><li class="mui-table-view-cell mui-media"><a class="mui-navigate-right"id="wodeyihu"><div class="tubiao"><img class="mui-media-object mui-pull-left"src="../images/item_xiangqing/icon_refund.png" style=" width:0.39rem;height:0.46rem"></div><div class="mui-media-body"style="" v-on:click="greet(post.value)">{{post.label}}</div></a></li></ul></div>'
var header3 = '<div class="service_item_pc"><div class="mui-content-padded"style="text-align: center;">平台优势</div><ul class="mui-table-view mui-grid-view mui-grid-9"style="background-color: #FFFFFF;"><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/item_xiangqing/icon_service.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">职业服务&nbsp;专业服务</div></a></li><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/item_xiangqing/icon_security.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">医患投保&nbsp;全程安心</div></a></li><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/item_xiangqing/icon_customer_service.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">专业团队&nbsp;随时售后</div></a></li></ul><ul class="mui-table-view firstUl"id="table-viewX_1"><li class="mui-table-view-cell mui-media"><a class="mui-navigate-right"id="wodeyihu"><div class="tubiao"><img class="mui-media-object mui-pull-left"src="../images/item_xiangqing/icon_qualifications.png" style="width:0.42rem;height:0.42rem; vertical-align: text-bottom;"></div><div class="mui-media-body"style=""v-on:click="greet(post.value)">{{post.label}}</div></a></li></ul></div>'
var headerTemplate_text = '<div class="service_item_pc"><h3 class="head1 mui-card-header">{{post.label}}：</h3><div class="mui-card-content"><div class="mui-card-content-inner test-w3ls">{{post.value}}</div></div></div>'
var headerTemplate = '<div class="service_item_pc"><h3 class="head1 mui-card-header">{{post.label}}：</h3><div class="mui-card-content"><div class="mui-card-content-inner test-w3ls" v-if="post.displayType==\'html\'" v-html="post.value"></div><div class="mui-card-content-inner test-w3ls" v-else-if="post.displayType==\'img\'"><img v-bind:src="post.value|formatHttpString"></div><div class="mui-card-content-inner test-w3ls" v-else-if="post.displayType==\'text\'" >{{post.value}}</div></div></div>'

//var bottom_template='<div class="message bottom mui-hidden"><div class="icon"><span class="mui-icon mui-icon-info"></span></div><p class="content">应用当前版本过低，存在安全漏洞，请升级至最新版</p><div class="action"><button id="install"type="button"class="mui-btn mui-btn-blue">确定</button></div></div>'

Vue.component('my-item-content', {
	props: ['post', 'options', 'index'],
	template: headerTemplate,
	filters: {
		formatHttpString: function(str) {
			if(str != null && str != '') {
				return serverAddress + str.replace(/\\/g, "/");
			}
		},
		formatHttpStringHc: function(str) {
			var reg = new RegExp("\n", "g");
			if(str&&str!=null&&str!=""){
				return str.replace(reg, '<br>');
			}
		}
	},
	methods: {
		renderOption: function(option) {
			this.option = option
			return this.$interpolate(this.template)
		},
		greet: function(event) {
			// `event` 是原生 DOM 事件
			if(event) {
				//关闭消息框
				document.querySelector(".message").classList.remove("mui-hidden");
				document.querySelector(".content").innerHTML=event;
			}
		}
	},
	created:function(){
		var indexcurrent = this.index;
		if(indexcurrent == 1) {
			this.$options.template = header3;
		}
		if(indexcurrent == 2) {
			this.$options.template = header2;
		}
	}
})


/*通知详情*/


//var headerTemplate_img = '<div class="service_item_pc"><div class="mui-content-padded"style="text-align: center; padding-top:5px">平台优势</div><ul class="mui-table-view mui-grid-view mui-grid-9"style="background-color: #FFFFFF;"><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">职业服务&nbsp;专业服务</div></a></li><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">医患投保&nbsp;全程安心</div></a></li><li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4"><a href="#"class="3lie"><span class="mui-icon"><img class="qs-img-lazyload"src="../images/baogaojiedu_icon.png"style="max-width: 1rem;"></span><div class="mui-media-body"style="font-size: 0.2rem;">专业团队&nbsp;随时售后</div></a></li></ul><ul class="mui-table-view firstUl"id="table-viewX_1"><li class="mui-table-view-cell mui-media"><a class="mui-navigate-right"id="wodeyihu"><div class="tubiao"><img class="mui-media-object mui-pull-left"src="../images/wode/wdyh.png"></div><div class="mui-media-body"style="padding-top: 0.25rem;" v-on:click="greet(post.value)">{{post.label}}</div></a></li></ul><ul class="mui-table-view"id="table-viewX_2"><li class="mui-table-view-cell"><a class="mui-navigate-right"id="service_object"><div class="tubiao"><img class="mui-media-object mui-pull-left"src="../images/wode/ffdx.png"/></div><div class="mui-media-body"style="padding-top: 0.25rem;">{{post.cityCode}}</div></a></li></ul></div>'
//Vue.component('my-notice-xiangqing', {
//	props: ['post2'],
//	template: headerTemplate_img,
//	filters: {
//		
//	},
//	methods: {
//		
//	},
//	created(){
//		alert(333)
//	}
//})

/*柒拾精选组件*/


