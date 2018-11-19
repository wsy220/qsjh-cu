<div class="main-dc">
	<div class="mui-row">
		<div class="mui-col-xs-3">
			{{if(sub.userfaceimage)}}
			<img class="head-img" src="{{sub.userfaceimage |formatHttpString}}" /> {{else}}
			<img class="head-img" src="../images/txmrt.png" /> {{/if}}
		</div>
		<div class="mui-col-xs-6" style="padding-top: 3%;padding-left: 3%;">
			<div style="display: inline-flex;">
				{{sub.name}}
				<i style="padding-left: 5px"><img style="width: 24px;" src="{{sub.sex | signGenderPic}}" /></i>
			</div>
			<p style="padding-top: 3%;">{{sub.hid.name}}</p>
		</div>
		<div class="mui-col-xs-3" style="padding-top: 4%;">
			{{if(sub.level)}}
			<span class="dc-right">{{sub.level | formatChoiceType}}</span> {{else}}
			<span class="dc-right">医生</span> {{/if}}
		</div>
	</div>
	<div class="mui-row" style="margin-top: 5%;margin-bottom: 5%;">
		<h6>所属科室:
			{{if(sub.depId)}}
			<span class="color-black">{{sub.deptId.name}}</span>
			{{else}}
			<span class="color-black">无</span>
			{{/if}}
		</h6>
	</div>
	
	<h6>医生介绍：<span class="color-black">{{sub.introduction}}</span></h6>
</div>