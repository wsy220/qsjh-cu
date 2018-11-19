/**
 * 弹出选择列表插件
 * 此组件依赖 listpcker ，请在页面中先引入 mui.picker.css + mui.picker.js
 * varstion 1.0.1
 * by Houfeng
 * Houfeng@DCloud.io
 */

(function($, document) {

	//创建 DOM
	$.dom = function(str) {
		if(typeof(str) !== 'string') {
			if((str instanceof Array) || (str[0] && str.length)) {
				return [].slice.call(str);
			} else {
				return [str];
			}
		}
		if(!$.__create_dom_div__) {
			$.__create_dom_div__ = document.createElement('div');
		}
		$.__create_dom_div__.innerHTML = str;
		return [].slice.call($.__create_dom_div__.childNodes);
	};

	//定义弹出选择器类
	var PopPicker = $.PopPicker = $.Class.extend({
		//构造函数
		init: function(options) {
			var istishi;

			var self = this;
			self.options = options || {};

			istishi = self.options.tishi;

			if(istishi == "true") {
				//				var panelBuffer = '<div class="mui-poppicker">\
				//			<div class="mui-card mui-content-padded" style="position:absolute;z-index: 9;bottom:182px;width:100%;left:-10px;color:#848484;font-size:12px;line-height:1.2;    background-color: #ddd; ">\
				//				<div class="mui-content-padded">\
				//				<span class="mui-icon iconfont icon-tishi" style="margin-right:5px"></span>您已准备好药品，可以选择1小时以后的预约时间，请在医护上门服务前提供相应的药品。<br>\
				//				<span class="mui-icon iconfont icon-tishi" style="margin-right:5px"></span>如果您还没有准备好药品，可以选择8小时以后预约时间，请在医护上门服务前准备相应的药品。<br>\
				//				</div>\
				//			</div>\
				//			<div class="mui-poppicker-header">\
				//				<button class="mui-btn mui-poppicker-btn-cancel">取消</button>\
				//				<button class="mui-btn mui-btn-blue mui-poppicker-btn-ok">确定</button>\
				//				<div class="mui-poppicker-clear"></div>\
				//			</div>\
				//			<div class="mui-poppicker-body">\
				//			</div>\
				//		</div>';

				var panelBuffer = '<div class="mui-poppicker">\
		<div class="mui-poppicker-header">\
			<button class="mui-btn mui-poppicker-btn-cancel">取消</button>\
			<button class="mui-btn mui-btn-blue mui-poppicker-btn-ok">确定</button>\
			<div class="mui-poppicker-clear"></div>\
		</div>\
		<div class="mui-poppicker-body">\
		</div>\
	</div>';

			} else {
				var panelBuffer = '<div class="mui-poppicker">\
		<div class="mui-poppicker-header">\
			<button class="mui-btn mui-poppicker-btn-cancel">取消</button>\
			<button class="mui-btn mui-btn-blue mui-poppicker-btn-ok">确定</button>\
			<div class="mui-poppicker-clear"></div>\
		</div>\
		<div class="mui-poppicker-body">\
		</div>\
	</div>';
			}

			self.options.buttons = self.options.buttons || ['取消', '确定'];
			self.panel = $.dom(panelBuffer)[0];
			document.body.appendChild(self.panel);
			self.ok = self.panel.querySelector('.mui-poppicker-btn-ok');
			self.cancel = self.panel.querySelector('.mui-poppicker-btn-cancel');
			self.body = self.panel.querySelector('.mui-poppicker-body');
			self.mask = $.createMask();
			self.cancel.innerText = self.options.buttons[0];
			self.ok.innerText = self.options.buttons[1];
			self.cancel.addEventListener('tap', function(event) {
				self.hide();
			}, false);
			self.ok.addEventListener('tap', function(event) {
				if(self.callback) {
					var rs = self.callback(self.getSelectedItems());
					if(rs !== false) {
						self.hide();
					}
				}
			}, false);
			self.mask[0].addEventListener('tap', function() {
				self.hide();
//				self.dispose();
			}, false);
			self._createPicker();
			//防止滚动穿透
			self.panel.addEventListener($.EVENT_START, function(event) {
				event.preventDefault();
			}, false);
			self.panel.addEventListener($.EVENT_MOVE, function(event) {
				event.preventDefault();
			}, false);
		},
		_createPicker: function() {
			var pickerBuffer = '<div class="mui-picker">\
		<div class="mui-picker-inner">\
			<div class="mui-pciker-rule mui-pciker-rule-ft"></div>\
			<ul class="mui-pciker-list">\
			</ul>\
			<div class="mui-pciker-rule mui-pciker-rule-bg"></div>\
		</div>\
	</div>';

			var self = this;
			var layer = self.options.layer || 1;

			var width = (100 / layer) + '%';
			self.pickers = [];
			for(var i = 1; i <= layer; i++) {
				var pickerElement = $.dom(pickerBuffer)[0];
				pickerElement.style.width = width;
				self.body.appendChild(pickerElement);
				var picker = $(pickerElement).picker();
				self.pickers.push(picker);
				pickerElement.addEventListener('change', function(event) {
					var nextPickerElement = this.nextSibling;
					if(nextPickerElement && nextPickerElement.picker) {
						var eventData = event.detail || {};
						var preItem = eventData.item || {};
						nextPickerElement.picker.setItems(preItem.children);
					}
				}, false);
			}
		},
		//填充数据
		setData: function(data) {
			var self = this;
			data = data || [];
			self.pickers[0].setItems(data);
		},
		//获取选中的项（数组）
		getSelectedItems: function() {
			var self = this;
			var items = [];
			for(var i in self.pickers) {
				var picker = self.pickers[i];
				items.push(picker.getSelectedItem() || {});
			}
			return items;
		},
		//显示
		show: function(callback) {
			var self = this;
			self.callback = callback;
			var ui = self.ui;
			self.mask.show();
			document.body.classList.add($.className('poppicker-active-for-page'));
			self.panel.classList.add($.className('active'));

			//苹果BUG修复
//			ui.picker.style.display = 'block';
//			ui.picker.style.webkitTransform = 'translateY(0px)';
			//处理物理返回键
			self.__back = $.back;
			$.back = function() {
				self.hide();
			};
		},
		//隐藏
		hide: function() {
			var self = this;
			var ui=self.ui;
			if(self.disposed) return;
			self.panel.classList.remove($.className('active'));
			self.mask.close();
			document.body.classList.remove($.className('poppicker-active-for-page'));

			//修复苹果BUG
			setTimeout(function() {
//              ui.picker.style.webkitTransform = 'translateY(1300px)';
			}, 300);
			self.mask.close();
			//处理物理返回键
			$.back = self.__back;
		},
		dispose: function() {
			var self = this;
			self.hide();
			setTimeout(function() {
				if(self.panel.parentNode){
					self.panel.parentNode.removeChild(self.panel);
				}
				for(var name in self) {
					self[name] = null;
					delete self[name];
				};
				self.disposed = true;
			}, 300);
		}
	});

})(mui, document);