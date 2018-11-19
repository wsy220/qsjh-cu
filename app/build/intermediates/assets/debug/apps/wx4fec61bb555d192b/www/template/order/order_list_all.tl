{{each sub value index}}  
<div class="clear_float clear_float_top_one" style="width:100%;">
	<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL" data-order-type={{value.servicetype}}  id={{value.id}}>
		<li class="mui-table-view-cell">
			<div class="mui-table">
				<div class="mui-table-cell mui-col-xs-8">
					<span>{{value.createdOn}}</span>
				</div>
				<div class="mui-table-cell mui-col-xs-4">
					{{if value.dcstatus&&value.dcstatus!="0"&&value.dcstatus!="4"}}
						<span class="mui-h5"><span class="mui-text-right">已出发</span></span>
					{{/if}}
					{{if value.type=="unconfirm"}}
					<span class="mui-h5"><span class="mui-text-right">待确认</span></span>
					{{else if value.type=="unorder"}}
					<span class="mui-h5"><span class="mui-text-right">待接单</span></span>
					{{else if value.type=="non-payment"}}
					<span class="mui-h5"><span class="mui-text-right">待付款</span></span>
					{{ else if value.type=="waiting"&&value.servicetype==0}}
					<span class="mui-h5"><span class="mui-text-right">待服务</span></span>
					{{ else if value.type=="unanswer"}}
					<span class="mui-h5"><span class="mui-text-right">待解答</span></span>
					{{ else if value.type=="unevaluate"}}
					<span class="mui-h5"><span class="mui-text-right">待评价</span></span>
					{{ else if value.type=="refund"}}
					<span class="mui-h5"><span class="mui-text-right">待退款</span></span>
					{{ else if value.type=="finish"}}
					<span class="mui-h5"><span class="">已完成</span></span>
					{{/if}}
				</div>
			</div>
		</li>
		<li class="mui-table-view-cell">
			<div class="mui-table">
				<div class="mui-table-cell mui-col-xs-9" style="padding-right:15px" id="leftwenzi">
				<a href="#" data-order-type={{value.servicetype}}>
					<span class="mui-ellipsis">{{value.serviceitem}}</span>
					{{if value.servicetype==0}}
					<p>服务费&nbsp;{{value.price.serviceprice|formatJE}}
						/{{value.price.time}}次</p>
					{{else if value.servicetype==1}}
					<p>服务费&nbsp;{{value.price.totalprice|formatJE}}
						/1次</p>
					{{/if}}
				</div>
				{{if value.type=="paunconfirm"}}
				<div style=""><button style="background-color: #ff9e0d;font-size:12px;padding:5px; color:white" id="{{value.id}}">确认备品送达</button></div>
				{{/if}}
				</a>
			</div>
		</li>
	</ul>
</div>

{{/each}}
