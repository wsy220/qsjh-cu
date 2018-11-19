{{each sub value index}} 
<li class="no-list-dot mui-row">
	<div class="mui-col-xs-6">姓名：{{value.name}}</div>
	<div class="mui-col-xs-6">岗位：{{value.position | formatChoiceMenberTtpe}}</div>
</li>
{{/each}}
