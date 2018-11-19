{{each sub value index}}
<li class="mui-table-view-cell mui-media" id="{{value._id}}"  data-id="{{value._id}}" >

			<div class="mui-slider-right mui-disabled">
				<a class="mui-btn mui-btn-red" data-id={{value._id}}>删除</a>
			</div>



	<a class="cus-msg-box mui-slider-handle" href="javascript:;" data-id="{{value._id}}" data-type="{{value.template}}" data-link="" data-opentype="" data-ext="{{value.extras}}" data-isread="{{dataparent}}">
	{{if(value.toId.status=="0")}}
		<span><img class="mui-media-object mui-pull-left mui-media-object-img" type="{{value.type}}" src="{{@value.type | formatString 'notification'}}"><span class="mui-badge" id="red_dian" style="display:block"></span></span>
	{{else if(value.toId.status=="1")}}
	<span><img class="mui-media-object mui-pull-left" type="{{value.type}}" src="{{@value.type | formatString 'notification'}}"><span class="mui-badge" id="red_dian" style="display:none"></span></span>
	{{/if}}
		<div class="mui-media-body"  {{value._id | formatMediaBody msgFlag datestr value.createdOn}}">
			<span class="ziti">{{value.title}}</span>
			<span class="doctor mui-pull-right">{{value.createdOn | formatString 'datestr'}}</span>
			<p class="mui-ellipsis-3">{{value.content}}</p>
		</div>
	</a>
</li>
{{/each}}




