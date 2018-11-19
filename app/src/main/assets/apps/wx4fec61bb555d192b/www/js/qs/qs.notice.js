//安卓系统的服药提醒
var setcalendar = function() {
	mui.toast('功能加载中,请稍后', {
		type: 'div',
		duration: 1000
	});
};
var deletecalendar = function() {
	mui.toast('功能加载中,请稍后', {
		type: 'div',
		duration: 1000
	});
};
var calanderURL = 'content://com.android.calendar/calendars',
	eventsURL = 'content://com.android.calendar/events',
	ContentValues = plus.android.importClass("android.content.ContentValues"),
	Uri = plus.android.importClass('android.net.Uri'),
	ContentUris = plus.android.importClass('android.content.ContentUris'),
	Calendar = plus.android.importClass('java.util.Calendar'),
	main = plus.android.runtimeMainActivity(),
	userCursor = plus.android.invoke(main.getContentResolver(), 'query', Uri.parse(calanderURL), null, null, null, null),
	userCursor_count = plus.android.invoke(userCursor, 'getCount'),
	TimeZone = plus.android.importClass('java.util.TimeZone'),
	TimeZone_str = plus.android.invoke(TimeZone.getDefault(), 'getID');
setcalendar = function(title, description, date_str) {
	if(userCursor_count <= 0) { //如果没有日历账户
		var account = new ContentValues(),
			buildUpon = plus.android.invoke(Uri.parse(calanderURL), 'buildUpon'),
			CalendarContract = plus.android.importClass('android.provider.CalendarContract');
		plus.android.invoke(buildUpon, 'appendQueryParameter', CalendarContract.CALLER_IS_SYNCADAPTER, 'true');
		plus.android.invoke(buildUpon, 'appendQueryParameter', 'account_name', 'someone@something.com');
		plus.android.invoke(buildUpon, 'appendQueryParameter', 'account_type', 'com.android.exchange');
		//设置账户信息
		account.put('name', 'someone');
		account.put('account_name', 'someone@something.com');
		account.put('account_type', 'com.android.exchange');
		account.put('calendar_displayName', 'someone_calendar');
		account.put('visible', 1);
		account.put('calendar_color', '-9206951');
		account.put('calendar_access_level', '700');
		account.put('sync_events', 1);
		account.put('calendar_timezone', TimeZone_str);
		account.put('ownerAccount', 'someone@something.com');
		account.put('canOrganizerRespond', 0);
		//保存账户信息
		plus.android.invoke(main.getContentResolver(), 'insert', plus.android.invoke(buildUpon, 'build'), account);
		//重新定义userCursor
		userCursor = plus.android.invoke(main.getContentResolver(), 'query', Uri.parse(calanderURL), null, null, null, null);
		//重新定义userCursor_count
		userCursor_count++;
	}
	plus.android.invoke(userCursor, 'moveToLast');
	var calId = plus.android.invoke(userCursor, 'getString', plus.android.invoke(userCursor, 'getColumnIndex', '_id')),
		events = new ContentValues(),
		mCalendar = Calendar.getInstance(),
		date = date_str.split(/\s{1}|:|-/g);
	plus.android.invoke(mCalendar, 'set', Calendar.YEAR, ~~date[0]);
	plus.android.invoke(mCalendar, 'set', Calendar.MONTH, ((~~date[1]) - 1));
	plus.android.invoke(mCalendar, 'set', Calendar.DATE, ~~date[2]);
	plus.android.invoke(mCalendar, 'set', Calendar.HOUR_OF_DAY, ~~date[3]);
	plus.android.invoke(mCalendar, 'set', Calendar.MINUTE, ~~date[4]);
	var start = plus.android.invoke(plus.android.invoke(mCalendar, 'getTime'), 'getTime'),
		end = plus.android.invoke(plus.android.invoke(mCalendar, 'getTime'), 'getTime');
	//设置日历事件
	events.put('title', title);
	events.put('description', description);
	events.put('calendar_id', calId);
	events.put('dtstart', start);
	events.put('dtend', end);
	events.put('hasAlarm', 1);
	events.put('eventTimezone', TimeZone_str);
	var newEvent = plus.android.invoke(main.getContentResolver(), 'insert', Uri.parse('content://com.android.calendar/events'), events);
	var id = plus.android.invoke(newEvent, 'getLastPathSegment');
	var values = new ContentValues();
	values.put('event_id', id);
	values.put('minutes', '5');
	values.put('method', '1');
	plus.android.invoke(main.getContentResolver(), 'insert', Uri.parse('content://com.android.calendar/reminders'), values);
	mui.toast('设置提醒成功');
}
deletecalendar = function(title) {
	if(userCursor_count <= 0) { //如果没有日历账户
		var account = new ContentValues(),
			buildUpon = plus.android.invoke(Uri.parse(calanderURL), 'buildUpon'),
			CalendarContract = plus.android.importClass('android.provider.CalendarContract');
		plus.android.invoke(buildUpon, 'appendQueryParameter', CalendarContract.CALLER_IS_SYNCADAPTER, 'true');
		plus.android.invoke(buildUpon, 'appendQueryParameter', 'account_name', 'someone@something.com');
		plus.android.invoke(buildUpon, 'appendQueryParameter', 'account_type', 'com.android.exchange');
		//设置账户信息
		account.put('name', 'someone');
		account.put('account_name', 'someone@something.com');
		account.put('account_type', 'com.android.exchange');
		account.put('calendar_displayName', 'someone_calendar');
		account.put('visible', 1);
		account.put('calendar_color', '-9206951');
		account.put('calendar_access_level', '700');
		account.put('sync_events', 1);
		account.put('calendar_timezone', TimeZone_str);
		account.put('ownerAccount', 'someone@something.com');
		account.put('canOrganizerRespond', 0);
		//保存账户信息
		plus.android.invoke(main.getContentResolver(), 'insert', plus.android.invoke(buildUpon, 'build'), account);
	}
	//定义EventCursor用于删除
	var userCursor2 = plus.android.invoke(main.getContentResolver(), 'query', Uri.parse(eventsURL), null, null, null, null);

	deleteitem = title;
	plus.android.invoke(userCursor2, 'moveToFirst');
	while(!plus.android.invoke(userCursor2, 'isAfterLast')) {
		var eventTitle = plus.android.invoke(userCursor2, "getString", plus.android.invoke(userCursor2, 'getColumnIndex', 'title'));
		if(title == eventTitle) {
			var id = plus.android.invoke(userCursor2, 'getInt', plus.android.invoke(userCursor2, 'getColumnIndex', '_id'));
			var deleteUri = ContentUris.withAppendedId(Uri.parse(eventsURL), id);
			var rows = plus.android.invoke(main.getContentResolver(), "delete", deleteUri, null, null);
			if(rows == -1) {
				//事件删除失败
				mui.toast("删除失败");
				return;
			}
			mui.toast("删除成功");

		}
		plus.android.invoke(userCursor2, 'moveToNext');
	}
}