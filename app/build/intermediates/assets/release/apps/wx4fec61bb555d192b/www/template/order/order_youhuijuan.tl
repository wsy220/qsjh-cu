{{each sub value index}}
<ul class="mui-table-view title">
	<li class="mui-table-view-cell bg_white" id="{{value._id}}"><span class="mui-pull-left">-{{value.amount|formatJE}}代金券</span></li>
</ul>
{{/each}}
