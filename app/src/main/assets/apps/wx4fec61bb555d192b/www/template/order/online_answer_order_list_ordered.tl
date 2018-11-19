<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
{{if(data.states=="orderd" || data.states == "finish")}}
	<li class="mui-table-view-cell  qiangdan_hushi" id="qiangdan_hushi">
		{{if(data.servicepersonal!=null)}}
		<div class="mui-table-view-cell">
			<div class="mui-table">
				<div class="mui-table-cell mui-col-xs-10">
					<p class="ziti">系统根据您的情况为您推荐以下护士为您服务：</p>
				</div>
			</div>
		</div>
		<div class="mui-table-view-cell mui-row">
		
		<a href="#" class="mui-col-xs-6">
			<img class="mui-media-object" src="{{data.servicepersonal.userfaceimage|formatHttpString}}" id="head-img-doctor">
		</a>
		
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
			</ul>
		</div>
		</div>
		{{/if}}
	</li>
{{/if}}
</ul>

