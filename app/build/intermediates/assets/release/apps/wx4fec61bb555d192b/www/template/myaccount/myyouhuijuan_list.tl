{{each sub value index}}
{{if (value.type=="wsy")}}
<li class="mui-table-view-cell mui-media youhui_keyong">
	<div class="mui-row">
		<a href="javascript:;" class="mui-col-sm-8 mui-col-xs-8">
			<div class="mui-media-body p-rem2">
				<p><span class="B_f">{{value.amount|formatJENO2}}</span><span class="F_12_b">元&nbsp;&nbsp;{{value.couponName}}</span></p>
				<p class="F_10_b" style="margin-top: 10px;">{{value.startDate|formatDateEndDate}}-{{value.endDate|formatDateEndDate}}</p>
			</div>
		</a>
		<a href="javascript:;" class="mui-col-sm-4 mui-col-xs-4 text_FR" style="margin-right: -5px;">
			<div class="mui-media-body p-rem2">
				<span id="youhui_order"><img src="../images/myaccount/ljsy.png" style="width: 1.62rem; margin-top: -5px;margin-right: -0.2rem;"></span>
				<p class="F_10" style="margin-top: 20px;margin-right: -0.15rem;" id="active_instruction" data-id="{{value._id}}" data-content="{{value.instructions}}"><img src="../images/myaccount/sm.png" style="width: 0.17rem;">&nbsp;&nbsp;<span class="F_10 p_color" >活动说明</span></p>
			</div>
		</a>
	</div>
</li>

{{else if (value.type=="zyz")}}
<li class="mui-table-view-cell mui-media youhui_keyong">
	<div class="mui-row">
		<a href="javascript:;" class="mui-col-sm-8 mui-col-xs-8">
			<div class="mui-media-body p-rem2">
				<p><span class="B_f">{{value.amount|formatJENO2}}</span><span class="F_12_b">元&nbsp;&nbsp;{{value.couponName}}</span></p>
				<p class="F_10_b" style="margin-top: 10px;">{{value.startDate|formatDateEndDate}}-{{value.endDate|formatDateEndDate}}</p>
			</div>
		</a>
		<a href="javascript:;" class="mui-col-sm-4 mui-col-xs-4 text_FR" style="margin-right: -5px;">
			<div class="mui-media-body p-rem2">
				<span><img src="../images/myaccount/zyz.png" style="width: 1.62rem; margin-top: -5px;margin-right: -0.2rem;"></span>
				<p class="F_10" style="margin-top: 20px;margin-right: -0.15rem;"><img src="../images/myaccount/sm.png" style="width: 0.17rem;">&nbsp;&nbsp;<span class="F_10 p_color">活动说明</span></p>
			</div>
		</a>
	</div>
</li>

{{else if (value.type=="ygq")}}
<li class="mui-table-view-cell mui-media youhui_guoqi">
	<div class="mui-row">
		<a href="javascript:;" class="mui-col-sm-8 mui-col-xs-8">
			<div class="mui-media-body p-rem2">
				<p><span class="H_f">{{value.amount|formatJENO2}}</span><span class="F_12_h">元&nbsp;&nbsp;{{value.couponName}}</span></p>
				<p class="F_10_h" style="margin-top: 10px;">{{value.startDate|formatDateEndDate}}-{{value.endDate|formatDateEndDate}}</p>
			</div>
		</a>

	</div>
</li>
{{/if}}
{{/each}}