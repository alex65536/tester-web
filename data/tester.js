function checkFileSize(inputId, maxFileSize) {
	if (typeof FileReader !== 'undefined') {
		var inputElement = document.getElementById(inputId);
		var fileSize = inputElement.files[0].size;
		if (fileSize > maxFileSize * 1024) {
			window.alert(fileTooBigAlertHead + maxFileSize + fileTooBigAlertTail);
			inputElement.value = '';
		}
	}
}

function intToStr(x, minLen) {
	var str = '' + ~~x;
	while (str.length < minLen) {
		str = '0' + str;
	}
	return str;
}

function timeSecondsToStr(seconds) {
	var days = intToStr(seconds / 86400, 0);
	seconds %= 86400;
	var hours = intToStr(seconds / 3600, 2);
	seconds %= 3600;
	var minutes = intToStr(seconds / 60, 2);
	seconds = intToStr(seconds % 60, 2);
	var result = hours + ':' + minutes + ':' + seconds;
	if (days != 0) {
		result = days + 'd ' + result;
	}
	return result;
}

function attachTimer(element) {
	var timerObj = {};
	
	timerObj.callback = function() {
		var curTime = new Date().getTime();
		var passed = (curTime - timerObj.startTime) / 1000;
		var timerInterval = +timerObj.element.getAttribute('data-interval');
		var curValue = timerInterval - passed;
		if (curValue < 0) {
			curValue = 0;
			clearInterval(timerObj.timerId);
		}
		timerObj.element.textContent = timeSecondsToStr(curValue);
	}
	
	timerObj.element = element;
	timerObj.startTime = new Date().getTime();
	timerObj.timerId = setInterval(timerObj.callback, 100);
}

function attachTimers() {
	var elements = document.getElementsByClassName('timer');
	for (var i = 0; i < elements.length; i++) {
		attachTimer(elements.item(i));
	}
}
