{{each sub value index}}
<div class="youhui_active" id="youhui_active"  data-ext="3" data-forward="{{value.forwardtype}}"><img src="{{value.img|formatHttpString2}}"  data-lazyload="{{value.img|formatHttpString2}}" ></div>
{{/each}}