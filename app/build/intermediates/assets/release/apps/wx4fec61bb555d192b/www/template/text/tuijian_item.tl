<div class="top-order-right" id="close_x">
	<img src="../images/rob_img/sc.png" style="max-width: 0.75rem;" />
</div>


<ul id="UL_TABLE" class="mui-table-view mui-grid-view mui-grid-9" style="width: 85%;">
{{if(type)}}
{{each value_item value index}}
	<li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4" >
		<a href="#" id="{{value._id}}" data_name="{{value.name}}" data_type="item">
			<span class="mui-icon" ><img class="qs-img-lazyload" src="{{value.icon|formatHttpString2}}" data-lazyload="{{value.icon|formatHttpString2}}"></span>
			<div class="mui-media-body" style="font-size:0.3rem;margin-left:-3px">{{value.name}}</div>
		</a>
	</li>
{{/each}}



{{else if(!type)}}
{{each value_class value index}}
	<li class="mui-table-view-cell mui-media mui-col-xs-4 mui-col-sm-4" >
		<a href="#" id="{{value._id}}" data_name="{{value.nickname}}" data_typename={{value.typename}} data_type="class">
			<span class="mui-icon" style="margin-top:-5px" id="item_img"><img class="qs-img-lazyload" src="{{value.icon|formatHttpString2}}" data-lazyload="{{value.icon|formatHttpString2}}"></span>
			<span class="active_img displayNon" id="active_img"><img src="../images/rob_img/duihao.png" style="width: 0.3rem;"></span>
			<div class="mui-media-body" style="font-size:0.3rem;margin-left:-3px">{{value.nickname}}</div>
		</a>
	</li>
{{/each}}
{{/if}}
</ul>
{{if(type)}}
<div class="mui-content-padded" style="text-align: center; 12px; margin-left: 25px; width: 86%; position: absolute; bottom: 0.5rem;">
	<span style="color:#fb8332;" id="back">其他推荐<img src="../images/rob_img/fanhui_tuijian.png" width="20px" height="20px" style="margin-right: 10px;"></span>
</div>

{{else if(!type)}}
<div class="mui-content-padded" style="text-align: center; margin-left: 25px; width: 86%; position: absolute; bottom: 5px;">
	<button class="mui-btn mui-btn-outlined ">确定</button>
</div>
{{/if}}