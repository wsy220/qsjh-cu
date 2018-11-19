{{each sub value index}} 
<li style="height: 0.4rem; margin-top:2px;" class="mui-ellipsis ziticontent">
	<a href="" data-id="{{value._id}}" data-ext="{{value}}" class="mui-ellipsis lunboziticontent"><span><img src="../images/index_img/icon_new_information.png" style="width: 0.23rem;margin-right: 5px;vertical-align: middle;"></span><span style="line-height:0.4rem">{{value.name}}</span></a>
</li>
{{/each}}
