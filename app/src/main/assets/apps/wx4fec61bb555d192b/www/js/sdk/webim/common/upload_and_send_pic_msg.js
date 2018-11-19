//弹出发图对话框
function selectPicClick() {
	//判断浏览器版本
	if(webim.BROWSER_INFO.type == 'ie' && parseInt(webim.BROWSER_INFO.ver) <= 9) {
		//if(1==1){
		$('#updli_form')[0].reset();
		$('#upload_pic_low_ie_dialog').modal('show');
	} else {
		$('#upd_form')[0].reset();
		var preDiv = document.getElementById('previewPicDiv');
		preDiv.innerHTML = '';
		var progress = document.getElementById('upd_progress'); //上传图片进度条
		progress.value = 0;
		$('#upload_pic_dialog').modal('show');
	}
}
//选择图片触发事件
function fileOnChange(uploadFile) {

	if(!window.File || !window.FileList || !window.FileReader) {
		alert("您的浏览器不支持File Api");
		return;
	}

	var file = uploadFile.files[0];
	var fileSize = file.size;

	//先检查图片类型和大小
	if(!checkPic(uploadFile, fileSize)) {
		return;
	}

	//预览图片
	var reader = new FileReader();
	var preDiv = document.getElementById('previewPicDiv');
	reader.onload = (function(file) {
		return function(e) {
			preDiv.innerHTML = '';
			var span = document.createElement('span');
			span.innerHTML = '<img class="img-responsive" src="' + this.result + '" alt="' + file.name + '" />';
			//span.innerHTML = '<img class="img-thumbnail" src="' + this.result + '" alt="' + file.name + '" />';
			preDiv.insertBefore(span, null);
		};
	})(file);
	//预览图片
	reader.readAsDataURL(file);
}

//上传图片进度条回调函数
//loadedSize-已上传字节数
//totalSize-图片总字节数
//function onProgressCallBack(loadedSize, totalSize) {
//  var progress = document.getElementById('upd_progress');//上传图片进度条
//  progress.value = (loadedSize / totalSize) * 100;
//}

//上传图片
function uploadPic(to_account, file) {
	//var uploadFiles = document.getElementById('upd_pic');
	//var file = uploadFiles.files[0];
//	alert("uploadPic " + to_account);
//	alert("uploadPic " + JSON.stringify(file));
	var businessType; //业务类型，1-发群图片，2-向好友发图片
	if(selType == webim.SESSION_TYPE.C2C) { //向好友发图片
		businessType = webim.UPLOAD_PIC_BUSSINESS_TYPE.C2C_MSG;
	} else if(selType == webim.SESSION_TYPE.GROUP) { //发群图片
		businessType = webim.UPLOAD_PIC_BUSSINESS_TYPE.GROUP_MSG;
	}
	//封装上传图片请求
	var opt = {
		'file': file, //图片对象
		'onProgressCallBack': onProgressCallBack, //上传图片进度条回调函数
		//'abortButton': document.getElementById('upd_abort'), //停止上传图片按钮
		'To_Account': to_account, //接收者
		'businessType': businessType //业务类型
	};
//	alert("uploadPicImg " + JSON.stringify(opt));
	//上传图片
	webim.uploadPic(opt,
		function(resp) {
			//上传成功发送图片
			sendPic(resp, file.name);
			//$('#upload_pic_dialog').modal('hide');
		},
		function(err) {
			alert(err.ErrorInfo);
		}
	);
}

//上传图片(用于低版本IE)
function uploadPicLowIE() {
	var businessType; //业务类型，1-发群图片，2-向好友发图片
	if(selType == webim.SESSION_TYPE.C2C) { //向好友发图片
		businessType = webim.UPLOAD_PIC_BUSSINESS_TYPE.C2C_MSG;
	} else if(selType == webim.SESSION_TYPE.GROUP) { //发群图片
		businessType = webim.UPLOAD_PIC_BUSSINESS_TYPE.GROUP_MSG;
	}
	//封装上传图片请求
	var opt = {
		'formId': 'updli_form', //上传图片表单id
		'fileId': 'updli_file', //file控件id
		'To_Account': selToID, //接收者
		'businessType': businessType //图片的使用业务类型
	};
	webim.submitUploadFileForm(opt,
		function(resp) {
			$('#upload_pic_low_ie_dialog').modal('hide');
			//发送图片
			sendPic(resp);
		},
		function(err) {
			$('#upload_pic_low_ie_dialog').modal('hide');
			alert(err.ErrorInfo);
		}
	);
}

//上传图片(通过base64编码)
function uploadPicByBase64(to_account, filepath, msgObject,selType) {

	var businessType; //业务类型，1-发群图片，2-向好友发图片
	if(selType == webim.SESSION_TYPE.C2C) { //向好友发图片
		businessType = webim.UPLOAD_PIC_BUSSINESS_TYPE.C2C_MSG;
	} else if(selType == webim.SESSION_TYPE.GROUP) { //发群图片
		businessType = webim.UPLOAD_PIC_BUSSINESS_TYPE.GROUP_MSG;
	}

	resize(filepath, function(file) {
		getFileMd5(file.target, function(md5, nb) {
			//console.log(nb)
			var datastr = nb.split(',', 3)
			var pics = [];
			if(datastr.length > 1) {
				pics.push(datastr[1]);
			} else {
				pics.push(datastr[0]);
			}
			//			console.log("result-md5", md5)
			//			console.log("result", pics[0])
			//			console.log("result.size", file.size)
			var opt = {
				'toAccount': to_account, //接收者
				'businessType': businessType, //图片的使用业务类型
				'fileMd5': md5,
				'totalSize': file.size, //图片大小,Byte
				'base64Str': pics[0]
			};
			//console.log("opt----------------------------", JSON.stringify(opt))
			webim.uploadPicByBase64(opt,
				function(resp) {
					console.log("resp " + resp);
					var test = sendPic(resp, "测试", to_account);
				},
				function(err) {
					alert(err.ErrorInfo);
				}
			);
		});
	})

}
//发送图片消息
function sendPic(images, imgName, to_account) {
	selToID = to_account;
	if(!selToID) {
		alert("您还没有好友，暂不能聊天");
		return;
	}
	if(!selSess) {
		selSess = new webim.Session(selType, to_account, to_account, friendHeadUrl, Math.round(new Date().getTime() / 1000));
	}
	var msg = new webim.Msg(selSess, true, -1, -1, -1, webimLoginInfo.identifier, 0, webimLoginInfo.identifierNick);
	var images_obj = new webim.Msg.Elem.Images(images.File_UUID);
	for(var i in images.URL_INFO) {
		var img = images.URL_INFO[i];
		var newImg;
		var type;
		switch(img.PIC_TYPE) {
			case 1: //原图
				type = 1; //原图
				break;
			case 2: //小图（缩略图）
				type = 3; //小图
				break;
			case 4: //大图
				type = 2; //大图
				break;
		}
		newImg = new webim.Msg.Elem.Images.Image(type, img.PIC_Size, img.PIC_Width, img.PIC_Height, img.DownUrl);
		images_obj.addImage(newImg);
	}
	msg.addImage(images_obj);
	//if(imgName){
	//    var data=imgName;//通过自定义消息中的data字段保存图片名称
	//    var custom_obj = new webim.Msg.Elem.Custom(data, '', '');
	//    msg.addCustom(custom_obj);
	//}
	//调用发送图片消息接口
	webim.sendMsg(msg, function(resp) {
		if(selType == webim.SESSION_TYPE.C2C) { //私聊时，在聊天窗口手动添加一条发的消息，群聊时，长轮询接口会返回自己发的消息
			//alert(msg);
			//addMsg(msg);
			//alert("webim.sendMsg " + convertMsgtoHtml(msg));

			msgbody = {
				"MsgType": "TIMImageElem",
				"MsgContent": {
					"UUID": "1853095_D61040894AC3DE44CDFFFB3EC7EB720F",
					"ImageFormat": 1,
					"ImageInfoArray": [{
							"Type": 1, //原图
							"Size": 1853095,
							"Width": 2448,
							"Height": 3264,
							"URL": convertMsgtoHtml(msg)
						},
						{
							"Type": 2, //大图
							"Size": 2565240,
							"Width": 0,
							"Height": 0,
							"URL": convertMsgtoHtml(msg)
						},
						{
							"Type": 3, //缩量图
							"Size": 12535,
							"Width": 0,
							"Height": 0,
							"URL": convertMsgtoHtml(msg)
						}
					]
				}
			}

			//sendMsg(to_account, msgbody);

			
		}
	}, function(err) {
		alert(err.ErrorInfo);
	});
}
//检查文件类型和大小
function checkPic(obj, fileSize) {
	var picExts = 'jpg|jpeg|png|bmp|gif|webp';
	var photoExt = obj.value.substr(obj.value.lastIndexOf(".") + 1).toLowerCase(); //获得文件后缀名
	var pos = picExts.indexOf(photoExt);
	if(pos < 0) {
		alert("您选中的文件不是图片，请重新选择");
		return false;
	}
	fileSize = Math.round(fileSize / 1024 * 100) / 100; //单位为KB
	if(fileSize > 30 * 1024) {
		alert("您选择的图片大小超过限制(最大为30M)，请重新选择");
		return false;
	}
	return true;
}

//单击图片事件
function imageClick(imgObj) {
	var imgUrls = imgObj.src;
	var imgUrlArr = imgUrls.split("#"); //字符分割
	var smallImgUrl = imgUrlArr[0]; //小图
	var bigImgUrl = imgUrlArr[1]; //大图
	var oriImgUrl = imgUrlArr[2]; //原图
	var bigPicDiv = document.getElementById('bigPicDiv');
	bigPicDiv.innerHTML = '';
	var span = document.createElement('span');
	span.innerHTML = '<img class="img-thumbnail" src="' + bigImgUrl + '" />';
	bigPicDiv.insertBefore(span, null);
	$('#click_pic_dialog').modal('show');
}

//压缩（需要获取本地文件权限）
function resize(src, callback) {
	var filename = src.substring(src.lastIndexOf('/') + 1);
	plus.zip.compressImage({
			src: src,
			dst: '_doc/' + filename,
			overwrite: true,
			//width: '1000px', //这里指定了宽度，同样可以修改
			format: 'jpg',
			quality: 80 //图片质量不再修改，以免失真
		},
		function(e) {
			callback(e);
		},
		function(err) {

		})
}

function getFileMd5(filePath, fc) {
	var str = ""
	plus.io.resolveLocalFileSystemURL(filePath, function(entry) {
		var fileReader = new plus.io.FileReader();
		fileReader.readAsDataURL(entry);
		fileReader.onloadend = function(evt) {
			var format = "file";
			//抽取DataURL中的数据部分，从Base64格式转换为二进制格式
			str = evt.target.result.split(',')[1]
			var bin = atob(evt.target.result.split(',')[1]);
			//创建空的Uint8Array
			var buffer = new Uint8Array(bin.length);
			//将图像数据逐字节放入Uint8Array中
			for(var i = 0; i < bin.length; i++) {
				buffer[i] = bin.charCodeAt(i);
			};
			//利用Uint8Array创建Blob对象
			blob = new Blob([buffer.buffer], {
				type: format
			});
			var fileReader1 = new FileReader();
			fileReader1.readAsBinaryString(blob);
			fileReader1.onload = function(evt) {
				if(evt.target.readyState == FileReader.DONE) {
					var imgblob = evt.target.result;
					var sparkMD5 = new SparkMD5();
					sparkMD5.appendBinary(imgblob);
					var MD5 = sparkMD5.end();
					if(fc)
						fc(MD5, str)
				}
			};
		}
	}, function(e) {
		console.log("Resolve file URL failed: " + e.message);
	});
}