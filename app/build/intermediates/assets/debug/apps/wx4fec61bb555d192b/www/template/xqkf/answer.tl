<div class="mui-slider-handle">
	<div class="mui-media-object mui-pull-right">
		<img id="head-img-user" class="head-img" src="{{userPic}}">
	</div>
	<div class="mui-media-body yh-answer">Q:&nbsp;{{ques}}</div>
</div>
<div class="mui-slider-handle">
	<div class="mui-media-object mui-pull-left">
		<img class="head-img" src="../../images/xiao7.png">
	</div>
	<div class="mui-media-body">
		<ul class="">
			<li class="answer-list">Q:&nbsp;{{sub.title}}</li>
			<li data-id="{{sub._id}}" class="answer-list">
				<div style="display: inline-flex;">
					<div>A:&nbsp;</div>
					<div>{{@sub.content}}</div>
				</div>
			</li>
		</ul>
	</div>
</div>