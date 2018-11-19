{{each sub value index}}  
<div class="clear_float clear_float_top_one" style="width:100%;">
	<ul class="mui-table-view kuan_qi left mui-confirm-button" data-id="{{value._id}}">
		<li class="mui-table-view-cell kuan_one kuan_two">
			<span class="cus_title_01">预约时间:{{value.timeinterval.intervalbegin | formatIntervalStart }}-{{value.timeinterval.intervalend | formatIntervalEnd }}</span>
		</li>
		
		<li class="mui-table-view-cell">
			<span class="mui-icon iconfont icon-je " style="color:orange"></span> <strong>订&nbsp;单&nbsp;号&nbsp;：</strong><span>{{value.ordernum}}</span>
		</li>
		<li class="mui-table-view-cell">
			<span class="mui-icon iconfont icon-je " style="color:orange"></span> <strong>发&nbsp;布&nbsp;者&nbsp;：</strong><span>{{value.username}}</span>
		</li>
		<li class="mui-table-view-cell">
			<span class="mui-icon iconfont icon-je " style="color:orange"></span> <strong>地&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;址&nbsp;：</strong><span>{{value.customer.homeaddress  | formatStringLan 20 }}</span>
		</li>
		<li class="mui-table-view-cell">
			<span class="mui-icon iconfont icon-je " style="color:orange"></span> <strong>服&nbsp;务&nbsp;费&nbsp;：</strong><span style="color:red;">{{value.price.serviceprice}}/{{value.price.time}}次</span>
		</li>
		<li class="mui-table-view-cell">
			<span class="mui-icon iconfont icon-fwxm " style="color:red"></span> <strong>服务项目：</strong> <span>{{value.serviceitem}}</span>
		</li>
		<li class="mui-table-view-cell">
			<span class="mui-icon iconfont icon-fbz" style="color:green"></span> <span class="black_tip">匿名打电话</span>
		</li>
	</ul>
	<ul class="dot_qiangdan_dot_one left">
		<li class="half_one">{{value.orderSericedCount}}/{{value.price.time}}次服务</li>
		<li class="peixun_one">培训系统</li>
		<li class="yingji_one">应急培训</li>
	</ul>
</div>
{{/each}}