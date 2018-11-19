
<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
	{{if(sub.question.pictures.length!=0)}}
	<li class="mui-table-view-cell" id="yaopinLI">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-10">
				<p class="ziti">检验报告：</p>
				<div id='image-list-yaopin' class="row image-list feedback">
					{{each sub.question.pictures value index}}
					<div class="image-item space"><div class=""><div class="content"><a href="#"><img src="{{value|formatHttpString}}" data-preview-src="" data-preview-group="1" class="mui-action-preview head-img"></a></div></div></div>
					{{/each}}
				</div>
			</div>
		</div>
	</li>
	{{/if}}
	
	<li class="mui-table-view-cell">
		<div class="mui-table">
			<div class="mui-table-cell mui-row">
				<p class="ziti2 mui-col-xs-6">服务费：</p>
				<p class="ziti2 mui-pull-right mui-col-xs-6 tR" id="heji">{{sub.price.servicetotalprice|formatJE}}元</p>
				{{if(iscoupon)}}
				<p class="ziti2 mui-col-xs-6">优惠券：</p>
				<p class="ziti2 mui-pull-right mui-col-xs-6 tR" id="youhuijuan">{{sub.coupon.amount|formatJE}}元</p>
				<p class="ziti2 mui-col-xs-6">合计：</p>
				<p class="ziti2 mui-pull-right mui-col-xs-6 tR" id="heji">{{(sub.price.totalprice-sub.coupon.amount)|formatJE}}元</p>
				{{else}}
				<p class="ziti2 mui-col-xs-6">合计：</p>
				<p class="ziti2 mui-pull-right mui-col-xs-6 tR" id="heji">{{sub.price.totalprice|formatJE}}元</p>
				{{/if}}
			</div>
		</div>
	</li>
	
</ul>

