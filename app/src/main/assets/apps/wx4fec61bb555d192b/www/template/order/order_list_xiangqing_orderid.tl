<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
	<li class="mui-table-view-cell">
		<div class="mui-table mui-pull-right">
			<div class="mui-table-cell">
				<span class="mui-h5">
					<span id="order_time">{{data.createdOn|formatDateEndDate}}</span>
				</span>
				{{if(data.servicetype==1&&data.states=="new")}}
				<span class="mui-h5">
					<span class="mui-pull-right" style="color:#f0ad4e">未接单</span>
				</span>
				{{/if}}
				{{if(data.servicetype==1&&data.states=="orderd")}}
				<span class="mui-h5">
					<span class="mui-pull-right" style="color:#f0ad4e">待解答</span>
				</span>
				{{/if}}
			</div>
		</div>
	</li>
	{{if(data.servicetype==0)}}
	<li class="mui-table-view-cell">
		<div class="mui-table">
			<div class="mui-table-cell" style="font-size:0.3rem">
				<span class="ziti">订单号：</span><span id="order_id">{{data.ordernum}}</span>
				{{if(type=="unconfirm")}}
				<span class="mui-badge mui-badge-huise1" id="button_status">退款</span>
				{{else if(type=="waiting"&&dcstatus<3)}}
				<span class="mui-badge mui-badge-huise1" id="button_status">退款</span>
				{{else if(type=="paunconfirm"&&dcstatus<3)}}
				<span class="mui-badge mui-badge-huise1" id="button_status">退款</span>
				{{else if(type=="unorder")}}
				<span class="mui-badge mui-badge-huise1" id="button_status">退款</span>
				{{else if(type=="non-payment")}}
				<span class="mui-badge mui-badge-huise" id="button_status">取消订单</span>
				{{else if(type=="finish")}}
				<span class="mui-badge mui-badge-huise" id="button_status" style="display:none">取消订单</span>
				{{else}}
				<span class="mui-badge mui-badge-huise1" id="button_status" style="display:none">退款</span>
				{{/if}}
				
			</div>
		</div>
	</li>
	{{/if}}
	
	{{if(data.servicetype==1)}}
	<li class="mui-table-view-cell">
		<div class="mui-table">
			<div class="mui-table-cell">
				<span class="ziti">订单号：</span><span id="order_id">{{data.ordernum}}</span>
				{{if(type=="unevaluate")}}
				<span class="mui-badge mui-badge-huise1" id="button_status" style="display:none">投诉</span>
				{{else if(type=="waiting"&&data.states=="new")}}
				<span class="mui-badge mui-badge-huise1" id="button_status">退款</span>
				{{else if(type=="waiting"&&data.states=="new")}}
				<span class="mui-badge mui-badge-huise1" id="button_status">退款</span>
				{{else if(type=="unorder")}}
				<span class="mui-badge mui-badge-huise1" id="button_status">取消订单</span>
				{{else if(type=="non-payment")}}
				<span class="mui-badge mui-badge-huise" id="button_status">取消订单</span>
				{{/if}}
				
			</div>
		</div>
	</li>
	{{/if}}
	
</ul>

