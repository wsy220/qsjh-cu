/*!
 * ======================================================
 * FeedBack Template For MUI (http://dev.dcloud.net.cn/mui)
 * =======================================================
 * @version:1.0.0
 * @author:cuihongbao@dcloud.io
 */
	var index = 1;
	var size = null;
	var imageIndexIdNum = 0;
	var starIndex = 0;
	var feedback = {
		question: document.getElementById('question'),
		contact: document.getElementById('contact'),
		imageList: document.getElementById('image-list'),
		submitBtn: document.getElementById('submit')
	};
	var url = 'https://service.dcloud.net.cn/feedback';
	feedback.files = [];
	feedback.uploader = null;
	feedback.deviceInfo = null;
	mui.plusReady(function() {
		//设备信息，无需修改
		feedback.deviceInfo = {
			appid: plus.runtime.appid,
			imei: plus.device.imei, //设备标识
			images: feedback.files, //图片文件
			p: mui.os.android ? 'a' : 'i', //平台类型，i表示iOS平台，a表示Android平台。
			md: plus.device.model, //设备型号
			app_version: plus.runtime.version,
			plus_version: plus.runtime.innerVersion, //基座版本号
			os: mui.os.version,
			net: '' + plus.networkinfo.getCurrentType()
		}
	});
	/**
	 *提交成功之后，恢复表单项 
	 */
	feedback.clearForm = function() {
		feedback.question.value = '';
		feedback.contact.value = '';
		feedback.imageList.innerHTML = '';
		feedback.newPlaceholder();
		feedback.files = [];
		index = 0;
		size = 0;
		imageIndexIdNum = 0;
		starIndex = 0;
		
	};
	feedback.getFileInputArray = function() {
		return [].slice.call(feedback.imageList.querySelectorAll('.file'));
	};
	feedback.addFile = function(path) {
		feedback.files.push({
			name: "images" + index,
			path: path,
			id: "img-" + index
		});
		index++;
	};
	/**
	 * 初始化图片域占位
	 */
	feedback.newPlaceholder = function(num) {
		var fileInputArray = feedback.getFileInputArray();
		console.log("FILEINPUT"+fileInputArray.length);
		if(fileInputArray.length>=parseInt(num)){
			return;
		}
		if(fileInputArray &&
			fileInputArray.length > 0
			&&fileInputArray[fileInputArray.length - 1].parentNode.classList.contains('space')
			) {
			return;
		};
		imageIndexIdNum++;
		var placeholder = document.createElement('div');
		placeholder.setAttribute('class', 'image-item space');
		var up = document.createElement("div");
		up.setAttribute('class', 'image-up');
		var down = document.createElement("div");
		down.setAttribute('class', 'content');
		down.textContent = "病情照片";
		//删除图片
		var closeButton = document.createElement('div');
		closeButton.setAttribute('class', 'image-close');
		closeButton.innerHTML = 'X';
		closeButton.id = "img-" + index;
		//小X的点击事件
		closeButton.addEventListener('tap', function(event) {
			setTimeout(function() {
				console.log("xiaoX"+feedback.files.length);
				for(var temp = 0; temp < feedback.files.length; temp++) {
					console.log("1"+feedback.files[temp].id);
					console.log("2"+closeButton.id);
					if(feedback.files[temp].id == closeButton.id) {
						feedback.files.splice(temp, 1);
						feedback.newPlaceholder(num);
					}
				}
				feedback.imageList.removeChild(placeholder);
			}, 0);
			return false;
		}, false);

		//
		var fileInput = document.createElement('div');
		fileInput.setAttribute('class', 'file');
		fileInput.setAttribute('id', 'image-' + imageIndexIdNum);
		fileInput.addEventListener('tap', function(event) {
			if(mui.os.plus) {
				var a = [{
					title: "拍照"
				}, {
					title: "从手机相册选择"
				}];
				plus.nativeUI.actionSheet({
					/*title: "修改用户头像",*/
					cancel: "取消",
					buttons: a
				}, function(b) { /*actionSheet 按钮点击事件*/
					switch(b.index) {
						case 0:
							break;
						case 1:
							getImagePhoto(num); /*拍照*/
							break;
						case 2:
							galleryImgPhoto(num); /*打开相册*/
							break;
						default:
							break;
					}
				})
			}
			var self = this;
			var index = (this.id).substr(-1);
			console.log("index"+index);
			function galleryImgPhoto(num) {
				plus.gallery.pick(function(e) {
					console.log("event:" + e);
					var name = e.substr(e.lastIndexOf('/') + 1);
					console.log("name:" + name);

					plus.zip.compressImage({
						src: e,
						dst: '_doc/' + name,
						overwrite: true,
						quality: 50
					}, function(zip) {
						uploadHead(e, 2, zip.width, zip.height);
						size += zip.size
						console.log("filesize:" + zip.size + ",totalsize:" + size);
						console.log("filewidth and fileheight" + zip.width + " " + zip.height);
						if(size > (10 * 1024 * 1024)) {
							return mui.toast('文件超大,请重新选择~');
							return false;
						}
						if(!self.parentNode.classList.contains('space')) { //已有图片
							feedback.files.splice(index - 1, 1, {
								name: "images" + index,
								path: e
							});
						} else { //加号
							placeholder.classList.remove('space');
							feedback.addFile(zip.target);
							feedback.newPlaceholder(num);
						}
						up.classList.remove('image-up');
						down.textContent = "";
						placeholder.style.backgroundImage = 'url(' + zip.target + ')';
					}, function(zipe) {
						mui.toast('压缩失败！')
					});

				}, function(e) {
					mui.toast(e.message);
				}, {});

			}

			function getImagePhoto(num) {
				var cmr = plus.camera.getCamera();
				var res = cmr.supportedImageResolutions[0];
				var fmt = cmr.supportedImageFormats[0];
				console.log("拍照res" + res);
				console.log("文件形式" + fmt);
				cmr.captureImage(function(p) {
					plus.io.resolveLocalFileSystemURL(p, function(entry) {
						var s = entry.toLocalURL() + "?version=" + new Date().getTime();
						uploadHead(s, 2, res.split("*")[0], res.split("*")[1]);
						//alert("真实路径：" + entry.fullPath);
						if(!self.parentNode.classList.contains('space')) { //已有图片
							feedback.files.splice(index - 1, 1, {
								name: "images" + index,
								path: entry.fullPath
							});
						} else { //加号
							placeholder.classList.remove('space');
							feedback.addFile(entry.fullPath);
							feedback.newPlaceholder(num);
						}
						up.classList.remove('image-up');
						down.textContent = "";
						placeholder.style.backgroundImage = 'url(' + entry.fullPath + ')';
					}, function(e) {
						alert(e.message);
					});
				}, function(e) {}, {
					filename: "_doc/camera/"
				});
			}
		}, false);

		placeholder.appendChild(closeButton);
		placeholder.appendChild(up);
		up.appendChild(down);
		placeholder.appendChild(fileInput);
		feedback.imageList.appendChild(placeholder);
	};
	//feedback.newPlaceholder(2);	

	
	

	//应用评分
	mui('.icons').on('tap', 'i', function() {
		var index = parseInt(this.getAttribute("data-index"));
		var parent = this.parentNode;
		var children = parent.children;
		if(this.classList.contains("mui-icon-star")) {
			for(var i = 0; i < index; i++) {
				children[i].classList.remove('mui-icon-star');
				children[i].classList.add('mui-icon-star-filled');
			}
		} else {
			for(var i = index; i < 5; i++) {
				children[i].classList.add('mui-icon-star')
				children[i].classList.remove('mui-icon-star-filled')
			}
		}
		starIndex = index;
	});
	//选择快捷输入
	mui('.mui-popover').on('tap', 'li', function(e) {
		document.getElementById("question").value = document.getElementById("question").value + this.children[0].innerHTML;
		mui('.mui-popover').popover('toggle')
	})
