<ul class="mui-table-view">
	<ul class="mui-table-view mui-table-view-striped mui-table-view-condensed tableUL">
		<li class="mui-table-view-cell libutton">
			<ul class="dowebok" id="dowebok">
			{{each sub value index}}
				<li><input type="checkbox" name="radio1" data-labelauty="{{value.department}}" value="{{value.disease}}" class="labelauty disease" id="labelauty" style="display: none;"><label for="labelauty"><span class="labelauty-unchecked" style="display:none">{{value.department}}</span><span class="labelauty-checked" style="display:inline-block;">{{value.disease}}</span></label></li>
			{{/each}}
			</ul>
		</li>
	</ul>
</ul>
