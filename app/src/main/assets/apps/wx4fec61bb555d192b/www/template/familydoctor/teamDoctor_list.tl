<li class="mui-table-view-cell"  style="padding: 4%;">
	<a class="" id="">
		<div class="mui-slider-handle mui-row">
			<div class="mui-col-xs-2 dcDetail" data-id="{{teamLeader._id}}" data-main="{{teamLeader}}">
				{{if(teamLeader.userfaceimage)}}
				<img class="" src="{{teamLeader.userfaceimage |formatHttpString}}" style="border-radius: 50%;"> {{else}}
				<img class="" src="../images/txmrt.png" style="border-radius: 50%;"> {{/if}}
			</div>
			<div class="mui-col-xs-8 dcDetail" data-id="{{teamLeader._id}}" data-main="{{teamLeader}}" style="padding:0 5%;">
				<div class="teamName">
					<span>{{teamLeader.name}}</span> {{if(teamLeader.level)}}
					<span class="type-dc">诊疗组长</span> {{else}}
					<span class="type-dc">医生</span> {{/if}}
				</div>
				<h6 class='mui-ellipsis-2'>擅长：{{teamLeader.introduction}}</h6>
			</div>
			<div class="mui-col-xs-2 go-chat" data-id="{{teamLeader._id}}"  data-name="{{teamLeader.name}}">
				<img class="chat-icon" src="../images/familydoctor/icon_chat(1).png">
			</div>
		</div>
	</a>
</li>
{{each sub value index}}
<li class="mui-table-view-cell " style="padding: 4%;">
	<a class="" id="">
		<div class="mui-slider-handle mui-row">
			<div class="mui-col-xs-2 dcDetail" data-id="{{value.doctor._id}}" data-main="{{value.doctor}}">
				{{if(value.doctor.userfaceimage)}}
				<img class="" src="{{value.doctor.userfaceimage |formatHttpString}}" style="border-radius: 50%;"> {{else}}
				<img class="" src="../images/txmrt.png" style="border-radius: 50%;"> {{/if}}
			</div>
			<div class="mui-col-xs-8 dcDetail" data-id="{{value.doctor._id}}" data-main="{{value.doctor}}" style="padding:0 5%;">
				<div class="teamName">
					<span>{{value.doctor.name}}</span> {{if(value.doctor.level)}}
					<span class="type-dc">{{value.doctor.level | formatChoiceType}}</span> {{else}}
					<span class="type-dc">医生</span> {{/if}}
				</div>
				<h6 class='mui-ellipsis-2'>擅长：{{value.doctor.introduction}}</h6>
			</div>
			<div class="mui-col-xs-2 go-chat"  data-id="{{value.doctor._id}}" data-name="{{value.doctor.name}}">
				<img class="chat-icon" src="../images/familydoctor/icon_chat(1).png">
			</div>
		</div>
	</a>
</li>
{{/each}}