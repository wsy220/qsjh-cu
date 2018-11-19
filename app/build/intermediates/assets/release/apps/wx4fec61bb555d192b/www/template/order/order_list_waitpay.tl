{{each sub value index}}  
<div class="clear_float clear_float_top_one" style="width:100%;">
	<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
		<li class="mui-table-view-cell">
			<div class="mui-table">
				<div class="mui-table-cell mui-col-xs-8">
					<span>{{value.ordertime}}</span>
				</div>
				<div class="mui-table-cell mui-col-xs-4">
				</div>
			</div>
		</li>
		<li class="mui-table-view-cell">
			<a href="#" id={{value.id}}>
				<div class="mui-table">
					<div class="mui-table-cell mui-col-xs-10">
						<span class="mui-ellipsis">{{value.serviceitem}}</span>
						<p>服务费&nbsp;{{value.price.serviceprice}}
							/{{value.price.time}}次</p>
					</div>
				</div>
			</a>
		</li>
	</ul>
</div>
{{/each}}
