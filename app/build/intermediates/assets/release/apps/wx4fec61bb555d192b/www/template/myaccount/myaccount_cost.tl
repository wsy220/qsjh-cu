{{each sub value index}}
<li class="mui-table-view-cell">
	<a class="mui-navigate-right1">
		<span class="mui-badge mui-badge-blue">已完成</span>{{value.ordertime|formatIntervalStart}}
	</a>
</li>
<li class="mui-table-view-cell mui-media">
	<a href="javascript:;">
		<div class="mui-media-body mui-ellipsis">
			{{value.serviceitem}}
			<p class="mui-ellipsis">服务费：{{value.price.totalprice/100}}元/{{value.price.time}}次</p>
		</div>
	</a>
</li>
{{/each}}
