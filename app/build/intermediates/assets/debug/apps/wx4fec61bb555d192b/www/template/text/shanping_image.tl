<div id="slider" class="mui-slider mui-fullscreen" style="background-color: white;">
	<div class="mui-slider-group">
	{{each sub value index}}
		<div class="mui-slider-item" >
			<div class="item-logo" style="background-image:url('{{value.img|formatHttpString2}}'); background-size: 100%; background-position: left top;"  data-ext="3" data-forward="{{value.forwardtype}}">
					<img src="{{value.img|formatHttpString2}}" height="100%" class="qs-img-lazyload" data-lazyload="{{value.img|formatHttpString2}}">
			</div>
		</div>
	{{/each}}
		
	</div>
	<div class="mui-slider-indicator">
		{{each sub value index}}
			<div class="mui-indicator"></div>
		{{/each}}
	</div>
</div>

