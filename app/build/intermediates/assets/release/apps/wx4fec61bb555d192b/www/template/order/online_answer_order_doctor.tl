{{if(type=="unevaluate"||type=="finish")}}
<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
	
	<li class="mui-table-view-cell">
		<div class="mui-table1">
			<div class="mui-table-cell mui-col-xs-10">
				<p class="ziti">医护回答：</p>
				<p class="neirong" id="question">
					{{data.answer.description}}
				</p>
			</div>
		</div>
	</li>
	{{if(data.answer.pictures)}}
	<li class="mui-table-view-cell" id="yaopinLI">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-10">
				<p class="ziti"></p>
				<div id='image-list-yaopin' class="row image-list feedback">
					{{each data.answer.pictures value index}}
					<div class="image-item space"><div class=""><div class="content"><a href="#"><img src="{{value|formatHttpString}}" data-preview-src="" data-preview-group="1" class="mui-action-preview head-img"></a></div></div></div>
					{{/each}}
				</div>
			</div>
		</div>
	</li>
	{{/if}}
</ul>
{{/if}}
