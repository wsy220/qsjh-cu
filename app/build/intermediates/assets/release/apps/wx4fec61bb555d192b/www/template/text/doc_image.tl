{{each sub value index}}
<li data-id="{{value.doctor._id}}" data-main="{{value.doctor}}">
	<div class="pic">
	
		{{if(value.doctor.level)}}
		<span class="mui-badge mui-badge-success">{{value.doctor.level | formatChoiceType}}</span>
		{{else}}
		<span class="mui-badge mui-badge-success">医生</span>
		{{/if}}
		
		
		{{if(value.doctor.userfaceimage)}}
		<div><img class="pic_img" src="{{value.doctor.userfaceimage |formatHttpString}}" /></div>
		{{else}}
		<div><img class="pic_img" src="../images/txmrt.png" /></div>
		{{/if}}
		
		
		<div class="title">
			<a href="#" target="_blank">{{value.doctor.name}}</a>
		</div>
		<div class="description">
			<a href="#" target="_blank">{{value.doctor.hid.name}}</a>
		</div>
	</div>
</li>
{{/each}}

