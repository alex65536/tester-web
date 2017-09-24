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
