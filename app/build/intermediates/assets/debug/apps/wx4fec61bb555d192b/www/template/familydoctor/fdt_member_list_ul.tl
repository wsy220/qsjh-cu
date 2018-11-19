<!--<ul class="mui-table-view">
	<li class="no-padding">
		<div class="background-pic">
			<div class="icon-team">
				<img class="icon-team-pic" src="{{arr.logo | formatHttpString}}" />
			</div>
			<div class="team-name">
				{{arr.logo}}
			</div>
		</div>
	</li>
	
</ul>-->
<ul class="mui-table-view firstUl" id="table-viewX">
	<li class="mui-table-view-cell">
		<div class="mingcheng" style="width: 20%;">服务期限</div>
		<div class="riqi">{{signDate}}</div>
	</li>
</ul>
<ul class="mui-table-view change-font-15" id="TEAMUL">
	<li class="mui-table-view-cell">
		<a class="mui-navigate-right teaminfo">团队介绍</a>
	</li>
	<li class="mui-table-view-cell hang" data-id="{{arr.teamLeader._id}}">
		<a class="mui-navigate-right">
			<div class="mingcheng" style="width: 20%;" data-memberType="general-practitioner" data-id="{{arr.teamLeader._id}}">{{arr.teamLeader.name}}</div>
			<div class="gangwei">队长、全科医生</div>
			<div class="goutong" data-name="{{arr.teamLeader.name}}" data-id="{{arr.teamLeader._id}}"> <img src="../images/familydoctor/u125.png" /></div>
		</a>
	</li>
	{{each arr.doctorList value index}}
	{{if value.doctor }}
	<li class="mui-table-view-cell hang" data-id="{{value.doctor._id}}">
		<a class="mui-navigate-right">
			<div class="mingcheng" style="width: 20%;" data-memberType="{{value.memberType}}" data-id="{{value.doctor._id}}">{{value.doctor.name}}</div>
			<div class="gangwei">{{value.memberType | formatChoiceMenberTtpe}}</div>
			<div class="goutong" data-name="{{value.doctor.name}}" data-id="{{value.doctor._id}}"> <img src="../images/familydoctor/u125.png" /></div>
		</a>
	</li>
	{{/if}}
	{{/each}}
</ul>