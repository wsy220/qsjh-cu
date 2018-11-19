// 旧头像
var imgData_old = "";
// 新头像
var imgData_new = "";
//上传图片
function uploadHead(imgPath, type, width, height) {
	if(type == 1) {
		head_image.src = imgPath;
		head_image.style.width = 200;
		head_image.style.height = 200;
		var image = new Image();
		image.src = imgPath;
		image.onload = function() {
			getBase64Image(image, type, 200, 200);
		};
	} else if(type == 2) {
		var image = new Image();
		image.src = imgPath;
		image.onload = function() {
			getBase64Image(image, type, width, height);
		};
	}

}
//将图片压缩转成base64 ,type为类型，1为头像，2为身份证
function getBase64Image(img, type, width, height) {
	var expectWidth = width;
	var expectHeight = height;
	var canvas = document.createElement("canvas");
	setTimeout(function() {
		var ctx = canvas.getContext("2d");
		canvas.width = expectWidth;
		canvas.height = expectHeight;
		ctx.drawImage(img, 0, 0, expectWidth, expectHeight);

		var mpImg = new MegaPixImage(img);
		EXIF.getData(img, function() {
			EXIF.getAllTags(this);
			/**
			 * 图片的旋转方向信息
			 * 1、图片没有发生旋转
			 * 6、顺时针90°
			 * 8、逆时针90°
			 * 3、180° 旋转
			 */
			var Orientation = EXIF.getTag(this, 'Orientation');
			if(Orientation != "" && Orientation != null) {
				// 方向信息，canvas 显示形式，canvas 对象，that,宽度，高度
				mpImg.render(canvas, {
					maxWidth: width,
					maxHeight: height,
					quality: 1,
					orientation: Orientation
				});
			}

			var dataURL = canvas.toDataURL("image/png", 1);
			imgData_new = dataURL.replace("data:image/png;base64,", "");
			
			if(type == 1) {
				touxiang_img.push(imgData_new);
				localStorage.setItem('HEAD_PHOTO', imgData_new);
				var list = plus.webview.currentWebview().opener();
				mui.fire(list, "refresh", {
					HEAD_PHOTO: imgData_new
				});
			} else if(type == 2) {
				shenfen_img.push(imgData_new);
			}
		});
	}, 1000);
}