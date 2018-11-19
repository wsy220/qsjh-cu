{{each sub value index}}  
<div class="clear_float clear_float_top_one" style="width:100%;">
	<ul class="mui-table-view mui-table-view-cus kuan_qi left mui-confirm-button-end" data-id="{{value._id}}">
		<li class="mui-table-view-cell kuan_one ">
			<span class="cus_title_01">预约时间:{{value.timeinterval.intervalbegin | formatIntervalStart }}-{{value.timeinterval.intervalend | formatIntervalEnd }}</span>
		</li>
		<li class="mui-table-view-cell ">
			<span class="mui-icon iconfont icon-hzdd" style="color:green"></span> <strong class="beaty">订&nbsp;单&nbsp;号&nbsp;：</strong> <span class="black_tip">{{value.ordernum}}</span>
		</li>
		<li class="mui-table-view-cell ">
			<span class="mui-icon iconfont icon-fbz" style="color:green"></span> <strong class="beaty">发&nbsp;布&nbsp;者&nbsp;：</strong> <span class="black_tip">{{value.username}}</span>
		</li>
		<li class="mui-table-view-cell ">
			<span class="mui-icon iconfont icon-dz" style="color:dodgerblue"></span> <strong class="beaty">地&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;址&nbsp;：</strong> 
			<span class="black_tip"> {{value.customer.homeaddress  | formatStringLan 24 }}</span>
		</li>
		<li class="mui-table-view-cell ">
			<span class="mui-icon iconfont icon-je " style="color:orange"></span> <strong class="beaty">服&nbsp;务&nbsp;费&nbsp;：</strong>
			<span style="color:red;">{{value.price.serviceprice}}/{{value.price.time}}次</span>
		</li>
		<li class="mui-table-view-cell ">
			<span class="mui-icon iconfont icon-fwxm " style="color:red"></span> <strong class="beaty">服务项目：</strong> <span class="black_tip">{{value.serviceitem}}</span>
		</li>
	</ul>
</div>
{{/each}}