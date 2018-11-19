{{each sub value index}}
<li class="mui-table-view-cell">
	<a class="mui-navigate-right1">
		{{if value.paymentstatus=="pbo"}}
		<span class="mui-badge2 mui-badge-huise">已退款</span>订单号：{{value.ordernum}}
		{{/if}}
		{{if value.paymentstatus=="wpb"}}
		<span class="mui-badge2 mui-badge-red">未退款</span>订单号：{{value.ordernum}}
		{{/if}}
	</a>
</li>
<li class="mui-table-view-cell mui-media">
	<a href="javascript:;">
		<div class="mui-media-body mui-ellipsis">
			退款金额：{{value.price.totalprice|formatFloatTK value.backScale}}元/{{value.price.time}}次
			<p class="mui-ellipsis">{{value.backData|formatIntervalStart}}</p>
		</div>
	</a>
</li>
{{/each}}
