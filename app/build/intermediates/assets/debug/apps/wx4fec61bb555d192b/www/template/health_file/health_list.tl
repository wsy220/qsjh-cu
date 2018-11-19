{{each sub value index}}
<li class="mui-table-view-cell mui-media health-detail" data-id="{{value._id}}" style="padding: 4%;">
	{{if value.relations !=="自己"}}
	<div class="mui-slider-right mui-disabled">
		<a class="mui-btn mui-btn-red" data-id="{{value._id}}">删除</a>
	</div>
	{{/if}}
	<a class="mui-navigate-right cus-msg-box mui-slider-handle" id="" style="margin-right: 0px;font-size: 14px;">
		<div class="mui-slider-handle" data-id="{{value._id}}">
			<img class="mui-media-object mui-pull-left" src="{{value.gender | genderPic}}" style="width:20px;height:20px">
			<div class="mui-media-body mui-row" style="padding-left: 5%;color:#737372 ;">
				<div class="mui-col-xs-3">{{value.realname}}</div>
				<div class="mui-col-xs-3">{{value.relations}}</div>

			</div>
		</div>
	</a>
</li>
{{/each}}