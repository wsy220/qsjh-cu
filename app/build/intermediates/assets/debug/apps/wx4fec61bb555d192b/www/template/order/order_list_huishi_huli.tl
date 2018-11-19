{{each orderjilu value index}}
<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL"  id="huli_jilu_UL">
	<li class="mui-table-view-cell">
		<div class="mui-table">
			<div class="mui-table-cell mui-col-xs-10">
				<p class="ziti">护理记录：<span class="mui-pull-right" id="hulilog_time">{{orderjilu.serviceLog.logTime|formatDateEndDate}}</span></p>
				<p class="neirong1" id="huli_jilu">{{orderjilu.serviceLog.logContent}}</p>
				<div id='image-list-huli_jilu' class="row image-list feedback">
					{{each orderjilu.serviceLog.logImg value index}}
						<div class="image-item space"><div class=""><div class="content"><a href="#"><img src="{{value|formatHttpString}}" data-preview-src="" data-preview-group="1" class="mui-action-preview head-img"></a></div></div></div>
					{{/each}}
				</div>
			</div>
		</div>
	</li>
</ul>
{{/each}}
