{{each sub value index}}	
	<a  data-ext="3" data-forward="{{sub[0].forwardtype}}" data-url="{{sub[0].url}}" data-name="{{sub[0].name}}" data-dataurl="{{sub[0].localUrl}}">
		<img src="{{sub[0].img|formatHttpString2}}">
	</a>
{{/each}}
	



