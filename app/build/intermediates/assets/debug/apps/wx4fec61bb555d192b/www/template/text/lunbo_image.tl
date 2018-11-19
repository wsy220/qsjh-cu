<div id="slider" class="mui-slider" style="height: 3.5rem;">
<div class="mui-slider-group mui-slider-loop">

<div class="mui-slider-item mui-slider-item-duplicate" data-ext="3" data-forward="{{sub[0].forwardtype}}" data-url="{{sub[0].url}}" data-name="{{sub[0].name}}" data-dataurl="{{sub[0].localUrl}}">
		<a href="#">
			<img class="qs-img-lazyload" style="height:3.5rem" src="{{sub[0].img|formatHttpString2}}" data-lazyload="{{sub[0].img|formatHttpString2}}">
		</a>
</div>
	
{{each sub value index}}	
	<div class="mui-slider-item" data-ext="3" data-forward="{{value.forwardtype}}" data-url="{{value.url}}" data-name="{{value.name}}" data-dataurl="{{value.localUrl}}">
		<a href="#">
			<img class="qs-img-lazyload" style="height:3.5rem" src="{{value.img|formatHttpString2}}" data-lazyload="{{value.img|formatHttpString2}}" >
		</a>
	</div>
{{/each}}	
	
<div class="mui-slider-item mui-slider-item-duplicate" data-forward="{{sub[sub.length-1].forwardtype}}" data-url="{{sub[sub.length-1].url}}" data-name="{{sub[sub.length-1].name}}" data-dataurl="{{sub[sub.length-1].localUrl}}">
		<a href="#">
			<img class="qs-img-lazyload" style="height:3.5rem" src="{{sub[sub.length-1].img|formatHttpString2}}" data-lazyload="{{sub[sub.length-1].img|formatHttpString2}}">
		</a>
</div>


</div>
<div class="mui-slider-indicator">
	{{each sub value index}}
	<div class="mui-indicator"></div>
	{{/each}}
</div>
</div>


