{{each drugmessage value index}} 
<div class="TUL">
<ul class="mui-table-view mui-table-view-chevron">
	<li id="switch" class="mui-table-view-cell">
		<img class="mui-media-object mui-pull-left" src="../images/xtxx.png"><span id="drug_name">{{value.drugname}}({{value.drugdw}})</span>
		{{if value.isopen=="true"}}
			<span class="mui-switch mui-active" data-id="{{index}}">
				<div class="mui-switch-handle"></div>
			</span>
		{{else if value.isopen=="false"}}
			<span class="mui-switch" data-id="{{index}}">
				<div class="mui-switch-handle"></div>
			</span>
		{{/if}}
		<img data-id="{{index}}" class="mui-media-object mui-pull-right msdel" src="../images/sc2.png" style="width:20px;height:20px;position: absolute;right: 10px;top: 20px;">
	</li>
</ul>
<ul class="mui-table-view">
	{{each value.drugtx value2 index}}
	<li class="mui-table-view-cell">
		<a class="mui-navigate-right">
			第{{index+1}}次：{{value2}}（服药时间）
		</a>
	</li>
	{{/each}}
</ul>
</div>
{{/each}}