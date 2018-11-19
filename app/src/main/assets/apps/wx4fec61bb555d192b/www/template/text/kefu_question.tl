{{each kefu_question value index}} 
<ul class="mui-table-view" id="table-viewX">
	<li class="mui-table-view-cell">
		<a class="mui-navigate-right" id="{{value._id}}">
			<div class="tubiao"><img class="mui-media-object mui-pull-left" src="../images/wode/kefu.png" /></div>
			<div class="mui-media-body" style="font-size:13px">
				{{value.value}}
			</div>
		</a>
	</li>
</ul>
{{/each}}