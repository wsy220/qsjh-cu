<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
{{if(data.states=="orderd" || data.states == "finish")}}
	<li class="mui-table-view-cell  qiangdan_hushi" id="qiangdan_hushi">
		<div class="mui-table-view-cell">
			<div class="mui-table">
				<div class="mui-table-cell mui-col-xs-10">
					<p class="ziti">系统根据您的情况为您推荐以下护士为您服务：</p>
				</div>
			</div>
		</div>
		<div class="mui-table-view-cell mui-row">
		{{if(data.servicepersonal!=null)}}
		<a href="#" class="mui-col-xs-6">
			<img class="mui-media-object" src="{{data.servicepersonal.userfaceimage|formatHttpString}}" id="head-img-doctor">
			<div class="mui-media-body">
			{{if (data.states!="finish")}}
			<button id="mapopen" class="viewweizhi"><img src="../images/wz.png" />查看位置</button>
			{{/if}}
			</div>
		</a>
		{{/if}}
		<div class="mui-media-body mui-col-xs-6">
			<ul style="margin-top:10px; padding-left:-10px">
				<li class="shanchang"><span id="serviceperson_name">{{data.servicepersonal.name}}</span> <span id="serviceperson_level">{{data.servicepersonal.dtype|ishushi}}</span><span class="mui-icon iconfont icon-aixinzhi" id="lovevalue">{{data.servicepersonal.lovevalue}}</span></li>
				<li class="shanchang">
				{{if data.servicepersonal.deptId}} 
					<span id="department">{{data.servicepersonal.deptId.name}}</span> 
				{{else if !data.servicepersonal.deptId}} 
					<span id="department">部门：无</span>
				{{/if}}
				<span id="hospitals">{{data.servicepersonal.hid.name}}</span>
				</li>
				{{if (data.states!="finish")}}
				<li class="shanchang mui-row m-rem5" id="twobutton_liuyan"><span class="mui-col-sm-6 mui-col-xs-6"><button class="liuyan" id="liuyan" data-id=""><img src="../images/familydoctor/u125.png" />留言</button></span><span class="mui-col-sm-6 mui-col-xs-6"><button class="tel" id="tel"><img src="../images/familydoctor/u97.png" />电话</button></span></li>
				{{/if}}
			</ul>
		</div>
		</div>
	</li>
{{/if}}
	<li class="mui-table-view-cell">
		<div>
			<div style="display: inline-block; width:65%">
				<p class="ziti" id="item_name">{{data.serviceitem}}</p>
			</div>	
			<div class=""  style="display: inline-block; float: right;">
				<p class="zitijg"><span id="serviceprice">{{data.price.servicetotalprice/100}}元/{{data.price.time}}次</span></p>
			</div>	
		</div>
		<div>	
			<div style="display: inline-block;">
				{{if(data.hasOwnProperty("coupon"))}}
				<p class="ziti" id="higher_name">优惠券价格:</p>
				{{/if}}
				
				{{if(!data.isprovidespares)}}
				<p class="ziti" id="higher_name">备品包价格:</p>
				{{/if}}
				
				<p class="ziti" id="heji_name">合计：</p>
			</div>	
			<div style="display: inline-block; float: right;">
				{{if(data.hasOwnProperty("coupon"))}}
					<p class="zitijg"><span id="youhuiprice">{{data.coupon.amount/100}}元</span></p>
					{{if(!data.isprovidespares)}}
						<p class="zitijg"><span id="serviceprice">{{data.orderSpares[0].groupprice/100}}元</span></p>
						<p class="zitijg"><span id="serviceprice">{{data.price.totalprice/100}}元</span></p>
					{{else}}
						<p class="zitijg"><span id="serviceprice">{{data.price.totalprice/100}}元</span></p>
					{{/if}}
				{{else}}
					{{if(!data.isprovidespares)}}
						<p class="zitijg"><span id="hejiprice">{{data.orderSpares[0].groupprice/100}}元</span></p>
						<p class="zitijg"><span id="hejiprice">{{data.price.totalprice/100}}元</span></p>
					{{else}}
						<p class="zitijg"><span id="hejiprice">{{data.price.totalprice/100}}元/{{data.price.time}}次</span></p>
					{{/if}}
				{{/if}}
			</div>
		</div>
		
		
	</li>
	{{if(data.orderImgs.recipeImgs.length!=0)}}
	<li class="mui-table-view-cell" id="chufangLI">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-10">          
				<p class="ziti" style="display:block">处方<span id="chufang_qixian"></span></p>
				<div id='image-list-chufang' class="row image-list feedback">
					{{each data.orderImgs.recipeImgs value index}}
					<div class="image-item space"><div class=""><div class="content"><a href="#"><img src="{{value|formatHttpString}}" data-preview-src="" data-preview-group="1" class="mui-action-preview head-img"></a></div></div></div>
					{{/each}}
				</div>
			</div>
		</div>
	</li>
	{{/if}}
	
	{{if(data.orderImgs.drugImgs.length!=0)}}
	<li class="mui-table-view-cell" id="yaopinLI">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-10">
				<p class="ziti">药品</p>
				<div id='image-list-yaopin' class="row image-list feedback">
					{{each data.orderImgs.drugImgs value index}}
					<div class="image-item space"><div class=""><div class="content"><a href="#"><img src="{{value|formatHttpString}}" data-preview-src="" data-preview-group="1" class="mui-action-preview head-img"></a></div></div></div>
					{{/each}}
				</div>
			</div>
		</div>
	</li>
	{{/if}}
	{{if(data.orderImgs.caseImgs.length!=0)}}
	<li class="mui-table-view-cell" id="caseLI" style="display:none;">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-10">
				<p class="ziti">病例、医嘱</p>
				<div id='image-list-case' class="row image-list feedback">
					{{each data.orderImgs.caseImgs value index}}
					<div class="image-item space"><div class=""><div class="content"><a href="#"><img src="{{value|formatHttpString}}" data-preview-src="" data-preview-group="1" class="mui-action-preview head-img"></a></div></div></div>
					{{/each}}	
				</div>
			</div>
		</div>
	</li>
	{{/if}}
	{{if(data.orderImgs.environmenImgs.length!=0)}}
	<li class="mui-table-view-cell" id="eventLI">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-10">
				<p class="ziti">居住环境</p>
				<div id='image-list-evment' class="row image-list feedback">
					{{each data.orderImgs.environmenImgs value index}}
					<div class="image-item space"><div class=""><div class="content"><a href="#"><img src="{{value|formatHttpString}}" data-preview-src="" data-preview-group="1" class="mui-action-preview head-img"></a></div></div></div>
					{{/each}}
				</div>
			</div>
		</div>
	</li>
	{{/if}}
	
	{{if(data.isneedspares == "0")}}
	<li class="mui-table-view-cell zitiTotal mui-collapse" id="already_queren3">
		<a class="mui-navigate-right ziti" href="#">备品</a>
		<div class="mui-collapse-content" id="already_queren4">
			<div class="mui-col-xs-12 mui-col-sm-12" id="Spares-list">
			{{each data.orderSpares value1}}
			<div style="background:#eeeeee" class="">{{value1.groupname}}:</div>
				{{each value1.spares value2 index}}
				<p class="neirong1 mui-row"><span class="zitijg2 mui-col-sm-8 mui-col-xs-8">{{value2.name}}</span><span class="mui-pull-right zitijg mui-col-sm-4 mui-col-xs-4">{{value2.standard}}×{{value2.number}}</span></p>
				{{/each}}
			{{/each}}
			</div>
		</div>
	</li>
	{{/if}}
</ul>
<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
	<li class="mui-table-view-cell">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-12 mui-col-sm-12">
				<p class="ziti">服务对象信息</p>
				<p class="neirong1 mui-row"><span class="">姓名：</span><span id="object_name">{{data.customer.realname}}</span></p>
				<p class="neirong1 mui-row"><span class="">性别：</span><span id="object_gender">{{data.customer.gender}}</span></p>
				<p class="neirong1 mui-row"><span class=" ">电话：</span><span id="object_tel">{{data.customer.tel}}</span></p>
				<p class="neirong1 mui-row"><span class=" ">出生日期：</span><span id="object_date">{{data.customer.idnumber|getBirthdayFromIdCard}}</span></p>
				<p class="neirong1 mui-row"><span class=" ">身份证号：</span><span id="object_idnumber">{{data.customer.idnumber}}</span></p>
				<p class="neirong1 " style="line-height:1.3"><span class="">地址：</span><span id="object_address" class="">{{data.customer.homeaddress}}</span></p>
				<p class="neirong1 mui-row" style="line-height:1.3">时间：<span id="object_time">{{data.timeinterval.intervalbegin|formatDate2}}—{{data.timeinterval.intervalend|formatDate2}}</span></p>
				<p class="neirong1 mui-row">服务：<span id="object_illness">{{data.serviceitem}}</span></p>
			</div>
		</div>
	</li>
</ul>
<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
	<li class="mui-table-view-cell">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-10">
				<p class="ziti">病情描述：</p>
				<p class="neirong1" id="illness_additional">{{data.additional.condition|formatmiaosu}}</p>
				<div id='image-list-illnessmiaoshu' class="row image-list feedback">
					{{each data.additional.pictureurl value index}}
						<div class="image-item space"><div class=""><div class="content"><a href="#"><img src="{{value|formatHttpString}}" data-preview-src="" data-preview-group="1" class="mui-action-preview head-img"></a></div></div></div>
					{{/each}}
				</div>
			</div>
		</div>
	</li>
</ul>

