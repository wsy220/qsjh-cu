var checkUrl = "/api/version/getnewversion/pa";


function checkUpdateForIndex(wgtVer, cf) {
	var sussess = function(data) {
		console.log("======================="+JSON.stringify(data))
		//服务器返回响应，根据响应结果，分析是否登录成功；
		if(data.result == "success") {
//			console.log(JSON.stringify(data.obj))
			plus.nativeUI.closeWaiting();
			var newVer = data.obj.version;
			var updateType = data.obj.updateType;
			var forceUpdate = data.obj.forceUpdate;

			if(wgtVer && newVer && (wgtVer != newVer)) {
				var inewVer = parseInt(newVer.replace(/\./g, "").replace(/V/i, ''));
				var iwgtVer = parseInt(wgtVer.replace(/\./g, "").replace(/V/i, ''));
				if(inewVer > iwgtVer) {
					if(updateType == "all") {
						if(forceUpdate && forceUpdate == "Y") {
							mui.alert('请确认更新应用版本' + newVer, '版本更新', function() {
								if(plus.os.name == "Android") {
									plus.runtime.openURL("http://sj.qq.com/myapp/detail.htm?apkName=io.dcloud.jiahucustomer");
								} else {
									plus.runtime.openURL("https://itunes.apple.com/cn/app/70%E4%BD%B3%E6%8A%A4/id1313168893?mt=8");
								}
							});
						} else {
							var btnArray = ['否', '是'];
							mui.confirm('是否更新应用到版本' + newVer, '下载更新', btnArray, function(e) {
								if(e.index == 1) {
									if(plus.os.name == "Android") {
										plus.runtime.openURL("http://sj.qq.com/myapp/detail.htm?apkName=io.dcloud.jiahucustomer");
									} else {
										plus.runtime.openURL("https://itunes.apple.com/cn/app/70%E4%BD%B3%E6%8A%A4/id1313168893?mt=8");
									}
								} else {
									cf()
								}
							})
						}
					} else {
						if(forceUpdate && forceUpdate == "Y") {
							mui.alert('请确认更新应用版本' + newVer + '(' + data.obj.size + 'M)', '版本更新', function() {
								downWgt(data.obj.path, inewVer); // 下载升级包
							});
						} else {
							var btnArray = ['否', '是'];
							mui.confirm('是否更新应用到版本' + newVer + '(' + data.obj.size + 'M)', '下载更新', btnArray, function(e) {
								if(e.index == 1) {
									downWgt(data.obj.path, inewVer); // 下载升级包
								} else {
									cf()
								}
							})
						}
					}
				} else {
					cf();
				}
			} else {
				cf();
			}
		} else {
			mui.toast(data.msg);
		}
	}

	var url = checkUrl + "?os=" + plus.os.name;
	commonHttpUtils(serverAddress+url, "get", {}, sussess, function() {
		plus.nativeUI.closeWaiting();
	}, function() {});
}

function checkUpdate(wgtVer, flag) {
	var sussess = function(data) {
		//服务器返回响应，根据响应结果，分析是否登录成功；
		if(data.result == "success") {
			plus.nativeUI.closeWaiting();
			var newVer = data.obj.version;
			var updateType = data.obj.updateType;
			var forceUpdate = data.obj.forceUpdate;

			if(wgtVer && newVer && (wgtVer != newVer)) {
				var inewVer = parseInt(newVer.replace(/\./g, "").replace(/V/i, ''));
				var iwgtVer = parseInt(wgtVer.replace(/\./g, "").replace(/V/i, ''));
				if(inewVer > iwgtVer) {
					if(updateType == "all") {
						if(forceUpdate && forceUpdate == "Y") {
							mui.alert('请确认更新应用版本' + newVer, '版本更新', function() {
								if(plus.os.name == "Android") {
									plus.runtime.openURL(serverAddress + data.obj.path.replace('client', '').replace(/\\/g, "/"));
								} else {
									plus.runtime.openURL("itms-apps://itunes.apple.com/cn/app/70%E4%BD%B3%E6%8A%A4-%E5%8C%BB%E6%8A%A4%E7%AB%AF/id1331357089?mt=8");
								}
							});
						} else {
							var btnArray = ['否', '是'];
							mui.confirm('是否更新应用到版本' + newVer, '下载更新', btnArray, function(e) {
								if(e.index == 1) {
									if(plus.os.name == "Android") {
										plus.runtime.openURL(serverAddress + data.obj.path.replace('client', '').replace(/\\/g, "/"));
									} else {
										plus.runtime.openURL("itms-apps://itunes.apple.com/cn/app/70%E4%BD%B3%E6%8A%A4-%E5%8C%BB%E6%8A%A4%E7%AB%AF/id1331357089?mt=8");
									}
								}
							})
						}
					} else {
						if(forceUpdate && forceUpdate == "Y") {
							mui.alert('请确认更新应用版本' + newVer + '(' + data.obj.size + 'M)', '版本更新', function() {
								downWgt(data.obj.path, inewVer); // 下载升级包
							});
						} else {
							var btnArray = ['否', '是'];
							mui.confirm('是否更新应用到版本' + newVer + '(' + data.obj.size + 'M)', '下载更新', btnArray, function(e) {
								if(e.index == 1) {
									downWgt(data.obj.path, inewVer); // 下载升级包
								}
							})
						}
					}
				} else {
					if(!flag)
						plus.nativeUI.alert("当前为最新版本！");
				}
			} else {
				if(!flag)
					plus.nativeUI.alert("当前为最新版本！");
			}
		}
	};
	var url = checkUrl + "?os=" + plus.os.name;
	commonHttpUtils(serverAddress+url, "get", {}, sussess, function() {
		plus.nativeUI.closeWaiting();
	}, function() {});
}

var wgtWaiting = null;

function downWgt(wgtUrl, newVer) {
	wgtUrl = wgtUrl.replace('client', '').replace(/\\/g, "/");
	wgtWaiting = plus.nativeUI.showWaiting("下载更新文件...");
	var task = plus.downloader.createDownload(serverAddress + wgtUrl, {
		filename: "_doc/update/" + newVer + "/"
	}, function(d, status) {
		if(status == 200) {
			installWgt(d.filename); // 安装wgt包
		} else {
			plus.nativeUI.alert("下载升级包失败！");
			plus.nativeUI.closeWaiting();
		}
	})
	task.addEventListener("statechanged", function(download, status) {
		switch(download.state) {
			case 2:
				wgtWaiting.setTitle("已连接到服务器");
				break;
			case 3:
				var percent = download.downloadedSize / download.totalSize * 100;
				wgtWaiting.setTitle("已下载 " + parseInt(percent) + "%");
				break;
			case 4:
				wgtWaiting.setTitle("下载完成");
				break;
		}
	});
	task.start();
}
// 更新应用资源
function installWgt(path) {
	plus.nativeUI.showWaiting("安装更新文件...");
	plus.runtime.install(path, {
		force: true
	}, function() {
		plus.nativeUI.closeWaiting();
		plus.nativeUI.alert("应用资源更新完成！", function() {
			//plus.webview.getLaunchWebview().close(true);
			plus.runtime.restart();
		});
	}, function(e) {
		plus.nativeUI.closeWaiting();
		plus.nativeUI.alert("安装更新文件失败[" + e.code + "]：" + e.message);
	});
}