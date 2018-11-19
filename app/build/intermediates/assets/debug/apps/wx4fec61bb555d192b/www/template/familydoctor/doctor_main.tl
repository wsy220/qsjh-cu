<div class="mui-row">
	<div class="mui-col-xs-3">
		<img class="head-img" src="{{sub.teamLeader.userfaceimage |formatHttpString}}" />
	</div>
	<div class="mui-col-xs-6" style="padding-top: 3%;padding-left: 3%;">
		<div style="display: inline-flex;">
			{{sub.teamLeader.name}}
			<i style="padding-left: 3%;"><img style="width: 24px;" src="{{sub.teamLeader.sex | signGenderPic}}" /></i>
		</div>
		<p style="padding-top: 3%;">{{sub.name}}</p>
	</div>
	<div class="mui-col-xs-3" style="padding-top: 4%;">
		{{if sub.teamLeader.level}}
		<span class="dc-right">{{sub.teamLeader.level | formatChoiceType}}</span> {{else}}
		<span class="dc-right">医生</span> {{/if}}
	</div>
</div>
<h6 style="display: inline-flex;">
					<i><img src="../images/familydoctor/icon_treatment.png" style="width: 16px;height: 16px;" /></i>
					擅长治疗
				</h6>
<h5>{{sub.teamLeader.introduction}}</h5>
<div class="mui-row" style="margin-top: 5%;">
	<div class="mui-col-xs-6" style="border-right: 1px solid #EBEBEB;">
		<h6>所属科室</h6> {{if sub.teamLeader.deptId}}
		<div id="">{{sub.teamLeader.deptId.name}}</div>
		{{else}}
		<div id="">无</div>
		{{/if}}
	</div>
	<div class="mui-col-xs-6" style="padding-left: 5%;">
		<h6>签约人数</h6>
		<div id="">{{signNum}}</div>
	</div>
</div>
<div style="margin-top: 5%;">
	<button id="signing" data-id="{{sub._id}}" class=" mui-btn mui-btn-color">签约团队</button>
</div>