{{each sub value index}}
					{{if value.dataType == 'bloodsugar'}}
						<li class="mui-table-view-cell">
							<div class="mui-table">
								<div class="mui-table-cell mui-col-xs-10">
									<p class="ziti">{{value.createdOn | formatDateEndDate}}</p>
									<p class="neirong1">早上：<span id="object_name"></span></p>
									<p class="neirong1">血糖餐前：<span id="object_tel">{{value.data && value.data.morning && value.data.morning.d1?value.data.morning.d1:"---"}}mmol/L</span></p>
									<p class="neirong1">血糖餐后2小时：<span id="object_address">{{value.data && value.data.morning && value.data.morning.d2?value.data.morning.d2:"---"}}mmol/L</span></p>
									<p class="neirong1">中午：<span id="object_name"></span></p>
									<p class="neirong1">血糖餐前：<span id="object_tel">{{value.data && value.data.noon && value.data.noon.d1?value.data.noon.d1:"---"}}mmol/L</span></p>
									<p class="neirong1">血糖餐后2小时：<span id="object_address">{{value.data && value.data.noon && value.data.noon.d2?value.data.noon.d2:"---"}}mmol/L</span></p>
									<p class="neirong1">晚上：<span id="object_name"></span></p>
									<p class="neirong1">血糖餐前：<span id="object_tel">{{value.data && value.data.night && value.data.night.d1?value.data.night.d1:"---"}}mmol/L</span></p>
									<p class="neirong1">血糖餐后2小时：<span id="object_address">{{value.data && value.data.night && value.data.night.d2?value.data.night.d2:"---"}}mmol/L</span></p>
									<p class="neirong1">血糖睡前：<span id="object_time">{{value.data && value.data.night && value.data.night.d3?value.data.night.d3:"---"}}mmol/L</span></p>
								</div>
							</div>
						</li>
					{{/if}}
					{{if value.dataType == 'bloodpressure'}}
						<li class="mui-table-view-cell">
							<div class="mui-table">
								<div class="mui-table-cell mui-col-xs-10">
									<p class="ziti">{{value.createdOn | formatDateEndDate}}</p>
									<p class="neirong1">早上：<span id="object_name"></span></p>
									<p class="neirong1">血压高压：<span id="object_tel">{{value.data && value.data.morning && value.data.morning.d2?value.data.morning.d2:"---"}}mmHg</span></p>
									<p class="neirong1">血压低压：<span id="object_tel">{{value.data && value.data.morning && value.data.morning.d1?value.data.morning.d1:"---"}}mmHg</span></p>
									<p class="neirong1">中午：<span id="object_name"></span></p>
									<p class="neirong1">血压高压：<span id="object_tel">{{value.data && value.data.noon && value.data.noon.d2?value.data.noon.d2:"---"}}mmHg</span></p>
									<p class="neirong1">血压低压：<span id="object_tel">{{value.data && value.data.noon && value.data.noon.d1?value.data.noon.d1:"---"}}mmHg</span></p>
									<p class="neirong1">晚上：<span id="object_name"></span></p>
									<p class="neirong1">血压高压：<span id="object_tel">{{value.data && value.data.night && value.data.night.d2?value.data.night.d2:"---"}}mmHg</span></p>
									<p class="neirong1">血压低压：<span id="object_tel">{{value.data && value.data.night && value.data.night.d1?value.data.night.d1:"---"}}mmHg</span></p>
								</div>
							</div>
						</li>
					{{/if}}
					{{if value.dataType == 'heartrate'}}
						<li class="mui-table-view-cell">
							<div class="mui-table">
								<div class="mui-table-cell mui-col-xs-10">
									<p class="ziti">{{value.createdOn | formatDateEndDate}}</p>
									<p class="neirong1">早上：<span id="object_name"></span></p>
									<p class="neirong1">心率：<span id="object_tel">{{value.data && value.data.morning && value.data.morning.d1?value.data.morning.d1:"---"}}次/分</span></p>
									<p class="neirong1">中午：<span id="object_name"></span></p>
									<p class="neirong1">心率：<span id="object_tel">{{value.data && value.data.noon && value.data.noon.d1?value.data.noon.d1:"---"}}次/分</span></p>
									<p class="neirong1">晚上：<span id="object_name"></span></p>
									<p class="neirong1">心率：<span id="object_tel">{{value.data && value.data.night && value.data.night.d1?value.data.night.d1:"---"}}次/分</span></p>
								</div>
							</div>
						</li>
					{{/if}}

{{/each}}
