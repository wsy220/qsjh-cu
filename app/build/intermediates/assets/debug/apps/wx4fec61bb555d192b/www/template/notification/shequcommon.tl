{{each sub value index}}
<li class="mui-table-view-cell mui-media" id="{{value._id}}" data-id="{{value._id}}">
	<a class="cus-msg-box mui-slider-handle" href="javascript:;" data-id="{{value._id}}">
		<div class="mui-media-body">
			<div class="mui-row">
				<span class="ziti mui-ellipsis mui-col-sm-7 mui-col-xs-7">{{value.name}}</span>
				<span class="doctor mui-pull-right mui-col-sm-5 mui-col-xs-5">{{value.createdOn | formatString 'datestr'}}</span>
			</div>
			<p class="mui-ellipsis-3 ">{{value.introduction}}</p>
		</div>
	</a>
</li>
{{/each}}




