

				<li class="mui-table-view-cell  mui-collapse">
					<a href="#" class="mui-navigate-right wureddian">
						<span id="beipinfuwu_wenzi" class=" ">{{beipin_message.groupname}}</span>
					</a>
					<div class="mui-collapse-content">
						<ul class="mui-table-view">
							<li class="mui-table-view-cell"><span class="mui-pull-left">种类</span><span class="mui-pull-right">规格/数量</span></li>
						</ul>

						<ul class="mui-table-view" id="mingxiUL">
							{{each beipin_message.spares value2 index2}}
							<li class="mui-table-view-cell"><span class="mui-pull-left mui-

col-sm-6" id="beipinzhonglei">{{value2.name}}</span><span class="mui-pull-right" id="beipinname">{{value2.standard}}/<span id="beipinnum">{{value2.number}}</span></span>
							</li>
							{{/each}}
						</ul>
						<ul  class="mui-table-view">
							<li class="mui-table-view-cell">
						
							<span class="mui-pull-right">总价：{{beipin_message.groupprice|formatJENO2}}元</span>
						
							</li>
						
						</ul>
						<ul class="mui-table-view">
							<li class="mui-table-view-cell" style="color:#FF9E0D" id="beipin_tishi_text">注：请您按照 备品清单在本次服务约定时间之前准备所需备品,否则医护人员有权拒绝服务，且在您本次服务费中扣除 30%作为医护人员补偿。

							</li>
						</ul>
					</div>

				</li>
				<li>
					<span class="mui-pull-right isbeipin mui-row" style="position: absolute;top:15px;right:30px; display: inline-block; ">
					
					<div style="display: inline-block;"><input id="leixingradio1" name="leixingradio" type="radio" value="0" checked="true"><label for="leixingradio1">自备</label></div>
					
					<div style="display: inline-block;"><input id="leixingradio2" name="leixingradio" type="radio" value="1"><label for="leixingradio2">平台代购</label></div>
					
				</span>
				</li>

