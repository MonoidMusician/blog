(function ($) {
	/* "YYYY-MM[-DD]" => Date */
	function strToDate(str) {
		return new Date(str);
	};

	/* Date => "YYYY-MM-DD" */
	if (typeof d3 === 'undefined') {
		function dateToStr(d) {
			return d.toISOString().split("T")[0];
		};
	} else {
		function dateToStr(d) {
			return d3.time.format.iso(d).split("T")[0];
		}
	}

	$.fn.calendar = function (options) {
		var _this = this;
		var opts = $.extend({}, $.fn.calendar.defaults, options);
		var week = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
		var tHead = week.map(function (day) {
			return "<th>" + day + "</th>";
		}).join("");

		_this.init = function () {
			var tpl = '<table class="cal">' +
			'<caption>' +
			'	<span class="prev"><a href="javascript:void(0);">&larr;</a></span>' +
			'	<span class="next"><a href="javascript:void(0);">&rarr;</a></span>' +
			'	<span class="month"><span>' +
			"</caption>" +
			"<thead><tr>" +
			tHead +
			"</tr></thead>" +
			"<tbody>" +
			"</tbody>" + "</table>";
			var html = $(tpl);
			_this.append(html);
		};

		function daysInMonth(d) {
			var newDate = new Date(d);
			newDate.setUTCMonth(newDate.getUTCMonth() + 1);
			newDate.setUTCDate(0);
			return newDate.getUTCDate();
		}

		_this.update = function (date) {
			var mDate = new Date(date);
			mDate.setUTCDate(1); /* start of the month */
			var day = mDate.getUTCDay(); /* value 0~6: 0 -- Sunday, 6 -- Saturday */
			mDate.setUTCDate(mDate.getUTCDate() - day) /* now mDate is the start day of the table */

			function dateToTag(d) {
				var tag = $('<td><a href="javascript:void(0);"></a></td>');
				var a = tag.find('a');
				a.text(d.getUTCDate());
				a.data('date', dateToStr(d));
				if (date.getUTCMonth() != d.getUTCMonth()) { // the bounday month
					tag.addClass('off');
				} else if (_this.data('date') == a.data('date')) { // the select day
					tag.addClass('active');
					_this.data('date', dateToStr(d));
				}
				var classes = opts.classes;
				if (classes) {
					if (typeof classes === 'function')
						classes = classes(d);
					if (typeof classes === 'string')
						classes = classes.split(' ');
					if (Array.isArray(classes))
						$.each(classes, function(i,c) {tag.addClass(c)});
				}
				return tag;
			};

			var tBody = _this.find('tbody');
			tBody.empty(); /* clear previous first */
			var cols = Math.ceil((day + daysInMonth(date))/7);
			for (var i = 0; i < cols; i++) {
				var tr = $('<tr></tr>');
				for (var j = 0; j < 7; j++, mDate.setUTCDate(mDate.getUTCDate() + 1)) {
					tr.append(dateToTag(mDate));
				}
				tBody.append(tr);
			}

			/* set month head */
			var monthStr = dateToStr(date).replace(/-\d+$/, '');
			_this.find('.month').text(monthStr);
		};

		_this.getCurrentDate = function () {
			return _this.data('date');
		}

		_this.init();
		/* in date picker mode, and input date is empty,
		 * should not update 'data-date' field (no selected).
		 */
		var initDate = opts.date? opts.date: new Date();
		if (opts.date || !opts.picker) {
			_this.data('date', dateToStr(initDate));
		}
		_this.update(initDate);

		/* event binding */
		_this.delegate('tbody td', 'click', function () {
			var $this = $(this);
			_this.find('.active').removeClass('active');
			$this.addClass('active');
			_this.data('date', $this.find('a').data('date'));
			/* if the 'off' tag become selected, switch to that month */
			if ($this.hasClass('off')) {
				_this.update(strToDate(_this.data('date')));
			}
			if (opts.picker) {  /* in picker mode, when date selected, panel hide */
				_this.hide();
			}
		});

		function updateTable(monthOffset) {
			var date = strToDate(_this.find('.month').text());
			date.setUTCMonth(date.getUTCMonth() + monthOffset);
			_this.update(date);
		};

		_this.find('.next').click(function () {
			updateTable(1);

		});

		_this.find('.prev').click(function () {
			updateTable(-1);
		});

		return this;
	};

	$.fn.calendar.defaults = {
		date: new Date(),
		picker: false,
		classes: '',
	};

	$.fn.datePicker = function () {
		var _this = this;
		var picker = $('<div></div>')
			.addClass('picker-container')
			.hide()
			.calendar({'date': strToDate(_this.val()), 'picker': true});

		_this.after(picker);

		/* event binding */
		// click outside area, make calendar disappear
		$('body').click(function () {
			picker.hide();
		});

		// click input should make calendar appear
		_this.click(function () {
			picker.show();
			return false; // stop sending event to docment
		});

		// click on calender, update input
		picker.click(function () {
			_this.val(picker.getCurrentDate());
			return false;
		});

		return this;
	};

	$(window).load(function () {
		$('.jquery-calendar').each(function () {
			$(this).calendar();
		});
		$('.date-picker:text').each(function () {
			$(this).datePicker();
		});
	});
}($));
