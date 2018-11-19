var lunboTM =
	'<div class="mui-slider" id="slider"  ref="slider" v-bind:style="{height:3.5rem}">\
		<div class="mui-slider-group mui-slider-loop">\
			<div class="mui-slider-item">\
				<a>\
					<div class="" v-bind:style="" v-lazy:background-image="" v-if="" v-bind:src="formatHttpString(post.img)"></div>\
				</a>\
			</div>\
		</div>\
		<div class="mui-slider-indicator" v-bind:class="" v-if="">\
			<div v-bind:class="" v-for="" v-if=""></div>\
		</div>\
	</div>';

Vue.component('lunbo-content', {
	props: ['post', 'options', 'index'],
	template: lunboTM,
	filters: {
		formatHttpString: function(str) {
			if(str != null && str != '') {
				return serverAddress + str.replace(/\\/g, "/");
			}
		},
		formatHttpStringHc: function(str) {
			var reg = new RegExp("\n", "g");
			if(str && str != null && str != "") {
				return str.replace(reg, '<br>');
			}
		}
	},
	methods: {
		renderOption: function(option) {
			this.option = option
			return this.$interpolate(this.template)
		},
		greet: function(event) {
			// `event` 是原生 DOM 事件
			if(event) {
				//关闭消息框
				document.querySelector(".message").classList.remove("mui-hidden");
				document.querySelector(".content").innerHTML = event;
			}
		},
		formatHttpString: function(str) {
			alert(333)
			if(str != null && str != '') {
				return serverAddress + str.replace(/\\/g, "/");
			}
		},
		formatHttpStringHc: function(str) {
			var reg = new RegExp("\n", "g");
			if(str && str != null && str != "") {
				return str.replace(reg, '<br>');
			}
		}
	},
	created: function() {
		console.log(formatHttpString(this.post.img));
	}
})