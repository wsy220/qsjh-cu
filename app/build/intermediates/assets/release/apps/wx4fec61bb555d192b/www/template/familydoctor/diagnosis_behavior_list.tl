{{each sub value index}}
<ul class="mui-table-view give-margin" data-id="{{value._id}}">
	<ul class="mui-table-view">
	    <li class="mui-table-view-cell font-bold">日期：{{value.servicedOn | formatIntervalStart}}</li>
	</ul>
	<ul class="mui-table-view">
	    <li class="mui-table-view-cell give-width">
	    	{{value.content}}
	    	<div class="mui-row">
	    		{{each value.contentImgList subvalue index}}
		    		<div class="pic-array-5">
		    			<img class="pic-fixed-size" src="{{subvalue | formatHttpString}}" alt="" data-preview-src="" data-preview-group="1"/>
		    		</div>
	    		{{/each}}
	    	</div>
	    </li>
    </ul>
</ul>

{{/each}}
