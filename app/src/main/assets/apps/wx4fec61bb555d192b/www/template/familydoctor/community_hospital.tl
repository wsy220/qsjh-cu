{{each sub value index}}
<li class="mui-table-view-cell hospitalDetail" data-id="{{value._id}}" style="padding: 4%;">
	<a class="" id="">
		<div class="mui-slider-handle" data-id="{{value._id}}">
			<img class=" mui-pull-left" src="{{value.hosImg | formatHttpString}}" style="width:2.3rem;height:1.5rem">
			<div class="mui-media-body " style="padding-left: 5%;">
				<div class="hospitalName">{{value.name}}</div>
				<div class="router">
					<i><img src="../images/familydoctor/icon_location.png" width="17px"></i>&nbsp;&nbsp;
					<span>{{value.distance |formatDistance}}</span>&nbsp;&nbsp;&nbsp;&nbsp;km
				</div>

			</div>
		</div>
	</a>
</li>
{{/each}}