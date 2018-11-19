{{each doc_sub value index}}
<li class="mui-table-view-cell mui-media">
	<a id={{value._id}}>
		<img class="mui-media-object mui-pull-left head-img mui-action-preview" style="width: 70px; height: 70px;" src="{{value.userfaceimage|formatHttpString}}">
		<div class="mui-media-body  mui-row"  style="padding-top: 3%;">
			<div class="shanchang mui-col-xs-3" style="border-right: 1px solid #CCCCCC;">
				<div>{{value.name}}</div>
				{{if value.deptId}}
				 <div>{{value.deptId.name}}</div>
				{{else}}
				 <div>未知</div>
				{{/if}}
			</div>
			<div class="shanchang mui-col-xs-5" style="border-right: 1px solid #CCCCCC;padding-left: 5%;">
				<div>{{value.dtype |ishushi}}</div>
				<div>{{value.hid.name}}</div>
			</div>
			<div class="shanchang mui-col-xs-4" style="padding-left: 3%;padding-top: 4%;">
				<span class="mui-icon iconfont icon-aixinzhi"></span> {{value.lovevalue}}
			</div>
		</div>
	</a>
</li>
{{/each}}