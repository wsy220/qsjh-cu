{{each sub value index}}
<div class="" style="margin: 2%;background-color: #FFFFFF;">
	<div class="mui-row">
		<div class="suger-num mui-col-xs-4">
			{{if value.xtsz <= 3.8}} 
				<div style="color:#F72735;" class="sugerNum">{{value.xtsz}}</div>
			{{/if}}
			{{if value.xtsz >= 11.1 }} 
				<div style="color:#F72735;" class="sugerNum">{{value.xtsz}}</div>
			{{/if}}
			{{if value.xtsz > 3.8 && value.xtsz < 11.1 }} 
				<div style="color:#10CC90;" class="sugerNum">{{value.xtsz}}</div>
			{{/if}}

		<div class="font-16" style="padding-bottom: 8%;">血糖值</div>
	</div>
	<div class="time mui-col-xs-8">
		<h6 class="device_num padding-5"></h6>
		<div class="font-16 padding-5">{{value.xgsjd}}</div>
		<div class="" style="display: inline-flex;width: 100%;">
			<div style="margin-top: 1px;">
				<img src="../images/healthdata/icon_clock_one.png" style="width: 14px;height: 14px;" />
			</div>
			<div class="timeNum">{{value.xtsj}}</div>
		</div>
	</div>
</div>
</div>
{{/each}}