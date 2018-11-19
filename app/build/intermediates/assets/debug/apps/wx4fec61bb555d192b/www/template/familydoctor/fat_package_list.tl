{{each sub value index}}
<div class="buyPackage" data-id={{value}}>
	<div class="mui-row uparea">
		<div class="mui-col-xs-8">
			<h4>{{value.title}}</h4>
			<h6>服务人群：{{value.targetPatient}}</h6>
		</div>
		<div class="mui-col-xs-4"><span>{{value.price|formatJENO2}}</span>元</div>
	</div>
	<div class="main-tip mui-row" style=" height: 39px; overflow: hidden;">
		<h6 class="mui-col-sm-7 mui-col-xs-7" style="line-height: 1.2; font-size:10px !important;">{{@value.content|formatHttpStringHc}}</h6>
		<h6 class="mui-col-sm-5 mui-col-xs-5" style="color: #4D4D4D; text-align: right; padding-top: 10px;font-size:10px;">查看更多...</h6>
	</div>
</div>
{{/each}}