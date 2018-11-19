{{each sub value index}}  
<li class="mui-table-view-cell"  style="padding: 4%;">

	<a class="" id="">
		<div class="mui-slider-handle mui-row" data-id="{{value._id}}">
			<div class="mui-col-xs-2 fdtTeam" data-id="{{value._id}}">
			
				<img class="" src="{{value.logo |formatHttpString}}" style="border-radius: 50%;height:1rem">
			</div>
			<div class="mui-col-xs-8 fdtTeam" data-id="{{value._id}}" style="padding:1% 5% 0;">
				<div class="teamName">{{value.name}}</div>
				<p style="padding-top: 1%;" class='mui-ellipsis-2'>{{value.summary}}</p>
			</div>
			<div class="mui-col-xs-2">
				<p class="signing" data-id="{{value._id}}" style="">签约 </p>
			</div>
		</div>
	</a>
</li>
{{/each}}