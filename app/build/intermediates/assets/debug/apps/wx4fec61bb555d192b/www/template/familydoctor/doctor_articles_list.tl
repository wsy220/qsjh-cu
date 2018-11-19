{{each sub value index}}  
<li class="mui-table-view-cell" data-id="{{value._id}}">
	<div class="mui-slider-handle" data-id="{{value._id}}">
		<img class="mui-media-object mui-pull-left" src="{{value.imgList[0] | formatHttpString}}" style="width:80px;height:80px">
		<div class="mui-media-body">
			{{value.articletitle}}
			<p class="mui-ellipsis-2" style="margin-top: 10px;">{{@value.articlecontent}}</p>
		</div>
	</div>
</li>
{{/each}}