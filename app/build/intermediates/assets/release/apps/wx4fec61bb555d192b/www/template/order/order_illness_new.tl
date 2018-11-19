<div class="mui-table-view" id="lby-demo">
	<div class="tableUL">
		<ul id="lby-checkbox-demo">
			{{each sub value index}}
				<li>
					<input class="to-labelauty-icon labelauty disease" type="checkbox" id="{{value.department}}" value="{{value.disease}}" style="display: none;"><label for="{{value.department}}">{{value.disease}}</label>
				</li>
			{{/each}}
		</ul>
	</div>
</div>