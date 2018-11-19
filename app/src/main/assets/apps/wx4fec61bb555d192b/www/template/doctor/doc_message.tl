<a class="" id="geren_message">
	<img class="mui-media-object mui-pull-left" id="head-img" src="{{message_sub.userfaceimage|formatHttpString}}">
	<div class="mui-media-body upimg">
		<span id="geren_name">{{message_sub.name}}{{message_sub.dtype|ishushi}}</span><br>
		<div class="vip-driver">
			<span class="mui-badge mui-badge-baihui mui-text-right" id="liuyan_button"><span class="mui-icon iconfont icon-xqliuyan"></span>留言</span>
			<span class="mui-badge mui-badge-baihui mui-text-right" id="tel_button"><span class="mui-icon iconfont icon-xqdianhua"></span>联系电话</span>
		</div>
		
		{{if message_sub.deptId|isJSONkong}}
		<div class="vip-driver">
			<span class="driver-dengji" id="totalscore_name">科室：{{message_sub.deptId.name}}</span>
		</div>
		{{/if}}
		<div class="vip-driver">
			<span class="driver-dengji" id="totalscore_name">医院：{{message_sub.hid.name}}</span>
		</div>
	</div>
</a>

