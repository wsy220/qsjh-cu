{{if ques}}
<div class="mui-slider-handle">
	<div class="mui-media-object mui-pull-right">
		<img id="head-img-user" class="head-img" src="{{userPic}}">
	</div>
	<div class="mui-media-body yh-answer">{{ques}}</div>
</div>
{{/if}}
<div class="mui-slider-handle">
	<div class="mui-media-object mui-pull-left">
		<img class="head-img" src="../../images/xiao7.png">
	</div>
	<div class="mui-media-body">
		<ul class="">
			{{if noSearch==true}} 
				{{if searchZero==true}}
					<li class="question-list">{{sub}}</li>
				{{else}} 
					{{each sub value index}}
					<li data-id="{{value._id}}" data-con="{{value.title}}" class="question-list">{{index+1}}.{{value.title}}</li>
					{{/each}} 
				{{/if}} 
			{{else}}
			<li class="question-list-click">
				<div style="text-align: center;width: 100%;">
					<a id="lastPage" class="mui-icon mui-icon-back mui-pull-left lastPage" style="color: #DDDDDD;font-size: 21px;"></a>
					热点问题
					<a id="nextPage" class="mui-icon mui-icon-forward mui-pull-right nextPage" style="color: #45d0d5;font-size: 21px;"></a>
				</div>
			</li>
			{{each sub value index}}
			<li data-id="{{value._id}}" data-con="{{value.title}}" class="question-list">{{index+1}}.{{value.title}}</li>
			{{/each}} {{/if}}
		</ul>
	</div>
</div>