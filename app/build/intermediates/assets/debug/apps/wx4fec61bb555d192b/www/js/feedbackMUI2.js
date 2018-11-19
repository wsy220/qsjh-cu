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
	if(fileInputArray.length >= parseInt(num)) {
		return;
	}
	if(fileInputArray &&
		fileInputArray.length > 0 &&
		fileInputArray[fileInputArray.length - 1].parentNode.classList.contains('space')
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
	down.textContent = "添加图片";
	//删除图片
	var closeButton = document.createElement('div');
	closeButton.setAttribute('class', 'image-close');
	closeButton.innerHTML = 'X';
	closeButton.id = "img-" + index;
	//小X的点击事件
	closeButton.addEventListener('tap', function(event) {
		setTimeout(function() {
			console.log("xiaoX" + feedback.files.length);
			for(var temp = 0; temp < feedback.files.length; temp++) {
				if(feedback.files[temp].id == closeButton.id) {
					feedback.files.splice(temp, 1);
					shenfen_img.splice(temp,1);
					feedback.newPlaceholder(num);
				}
			}
			feedback.imageList.removeChild(placeholder);
			for(var temp = 0; temp < feedback.files.length; temp++) {
				console.log("222" + JSON.stringify(feedback.files[temp]));

			}
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

		function galleryImgPhoto(num) {
			plus.gallery.pick(function(path) {
				zoomImage(path, "image" + Math.random() + ".jpg", num);
			}, function(a) {}, {
				filter: "image"
			})
		}

		function getImagePhoto(num) {
			var c = plus.camera.getCamera();
			c.captureImage(function(path) {
				zoomImage(path, "image" + Math.random() + ".jpg", num);
			});
		}

		//缩放图片
		function zoomImage(filename, name, num) {
			plus.nativeUI.showWaiting();
			plus.zip.compressImage({
					src: filename,
					dst: name,
					width: "30%",
					quality: 50,
					overwrite: true
				},
				function(event) {
					//alert("压缩图片成功：" + JSON.stringify(event));
					var task = plus.uploader.createUpload(serverAddress + "/api/uploadImg", {
							method: "POST",
							blocksize: 204800,
							priority: 100
						},
						function(t1, status) {
							// 上传完成
							if(t1.responseText != null) {
								var data1 = JSON.parse(t1.responseText);
								if(status == 200) {
									plus.nativeUI.closeWaiting();
									console.log(data1.result);
									if(data1.result == "success") {
										console.log("图片路径" + data1.obj.path);
										shenfen_img.push(data1.obj.path);
										if(!self.parentNode.classList.contains('space')) { //已有图片
											feedback.files.splice(index - 1, 1, {
												name: "images" + index,
												path: event
											});
										} else { //加号
											placeholder.classList.remove('space');
											feedback.addFile(event.target);
											feedback.newPlaceholder(num);
										}
										up.classList.remove('image-up');
										down.textContent = "";
										placeholder.style.backgroundImage = 'url(' + event.target + ')';
										for(var i = 0; i < shenfen_img.length; i++) {
											console.log("身份照片feedback"+shenfen_img[i]);
										}
									}
								} else {
									plus.nativeUI.closeWaiting();
									mui.alert("上传失败2: " + status);
								}
							}
						}
					);
					task.addFile(event.target, {
						key: "single-file"
					});
					task.start();
				},
				function(e) {
					plus.nativeUI.closeWaiting();
					alert("压缩图片失败: " + JSON.stringify(e));
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