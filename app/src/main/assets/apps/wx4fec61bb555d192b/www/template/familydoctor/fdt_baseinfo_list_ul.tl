<ul class="mui-table-view">
	<li class="no-padding">
		<div class="background-pic">
			<div class="icon-team">
			</div>
			<div class="team-name">
			</div>
		</div>
	</li>
	
</ul>
 	
<ul class="mui-table-view change-font-15" id="TEAMUL">
	<li class="mui-table-view-cell">
		<a class="mui-navigate-right btn_edit_fdt_info">团队介绍</a>
	</li>
	{{each arr value index}} 
			<li class="mui-table-view-cell hang" data-id="{{value._id}}">
					<a class="mui-navigate-right">
						<div class="mingcheng">{{value.title}}</div>
					</a>
			</li>
	{{/each}}
</ul>
