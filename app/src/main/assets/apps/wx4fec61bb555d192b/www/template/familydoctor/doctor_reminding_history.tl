{{each sub value index}}
<ul class="mui-table-view give-margin" data-id="{{value._id}}">
	    <li class="mui-table-view-cell">
	    <div class="mui-row">
	    	<img class="mui-pull-left" style="width:45px" src="../images/xtxx.png"/>
	    
	    	<h5 class="text-padding mui-pull-right"> {{value.content}}</h5>
	    </div>
	    </li>
	    <li class="mui-table-view-cell mui-row">
	    	<h5 class="mui-pull-right">{{value.servicedOn | formatIntervalStart}}</h5>
	    </li>
</ul>
{{/each}}