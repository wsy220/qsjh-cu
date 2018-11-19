<li class="no-list-dot mui-row">
	<div class="mui-col-xs-6">姓名：{{sub.teamLeader.name}}</div>
	<div class="mui-col-xs-6">岗位：诊疗组长</div>
</li>
{{each sub.doctorList value index}} 
<li class="no-list-dot mui-row">
	<div class="mui-col-xs-6">姓名：{{value.doctor.name}}</div>
	<div class="mui-col-xs-6">岗位：{{value.memberType | formatChoiceMenberTtpe}}</div>
</li>
{{/each}}
