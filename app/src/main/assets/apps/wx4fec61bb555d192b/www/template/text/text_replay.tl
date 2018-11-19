{{each subanswer value index}}  
<li class="mui-table-view-cell mui-media">
	<a href="javascript:;">
		{{if value.userId.userfaceimage}}
		<img class="mui-media-object mui-pull-left imgtouxiang" src="{{value.userId.userfaceimage|formatHttpString}}">
		{{else}}
		<img class="mui-media-object mui-pull-left imgtouxiang" src="../images/wode/txmrt.png">
		{{/if}}
		
		<div class="mui-media-body">
			{{value.userId.nickname}}
			<p class="yuanwen">{{value.content}}<br>{{value.timeCreated|formatDate}}</p>
			{{each value.sub value2 index2}}
				<p class="mui-ellipsis">
					<ul class="huifu2">
						<div class="mui-media-body huifu">
							{{if value2.userId.nickname !== value.userId.nickname}}
							<span class="huangzi">{{value2.userId.nickname}}{{value2.userId.dtype|ishushi}}</span>回复<span class="huangzi">@{{value.userId.nickname}}</span>
							{{/if}}
							{{if value2.userId.nickname == value.userId.nickname}}
							<span class="huangzi">{{value2.userId.nickname}}{{value2.userId.dtype|ishushi}}</span>回复<span class="huangzi">@{{value.sub[0].userId.nickname}}</span>
							{{/if}}
							<p class="mui-ellipsis">{{value2.content}}</p>
							<p class="mui-ellipsis">{{value2.timeCreated|formatDate}}</p>
						</div>
					</ul>
				</p>
			{{/each}}
		</div>
	</a>
</li>
{{/each}}
