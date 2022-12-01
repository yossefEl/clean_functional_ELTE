var taskCounter = -1;

function subset_select(name) {
	var left = document.getElementById(name + '-left');
	var right = document.getElementById(name + '-right');
	var value = document.getElementById(name + '-value');

	subset_transfer(left,right);
	subset_setvalue(right,value);
}
function subset_deselect(name) {
	var left = document.getElementById(name + '-left');
	var right = document.getElementById(name + '-right');
	var value = document.getElementById(name + '-value');

	subset_transfer(right,left);
	subset_setvalue(right, value);
}

function subset_transfer(from, to) {
	for(var i = 0; i < from.options.length; i++) {
		if(from.options[i].selected) {
			to.options[to.options.length] = new Option (from.options[i].text,from.options[i].value);
		}
	}
	for(var i = from.options.length - 1; i >= 0; i--) {
		if(from.options[i].selected) {
			from.remove(i);
		}
	}
}
function subset_setvalue(from, to) {
	var vals = new Array();
	for(var i = 0; i < from.options.length; i++) {
			vals[vals.length] = from.options[i].value;
	}
	to.value = vals.join("-");
}

function addTask(el) {
	var div = el;
	while (div.tagName != "DIV") {
		div = div.parentNode;
	}
	var table = div.previousSibling;
	var tbody = table.getElementsByTagName("TBODY")[0];
	var row = document.createElement("tr");

	var td1 = document.createElement("td");	
	var td1a = document.createElement("a");
	var td1img = document.createElement("img");
	td1img.src = "/icons/delete.png"
	td1img.alt = "Remove";
	td1a.href = "#";
	td1a.onclick = function () {delTask(this);}; 
	td1a.appendChild(td1img);	
	td1.appendChild(td1a);

	var td2 = document.createElement("td");
	td2.innerHTML = "-";

	var td3 = document.createElement("td");
	var td3ipt = document.createElement("input");
	td3ipt.name = "task_description-" + taskCounter;
	td3ipt.class = "pm-string";
	td3.appendChild(td3ipt);

	var td4 = document.createElement("td");	
	var td4ipt = document.createElement("input");
	td4ipt.type = "checkbox";
	td4ipt.name = "task_done-" + taskCounter;
	td4ipt.class = "pm-bool";
	td4ipt.value = "True";
	td4.appendChild(td4ipt);

	row.appendChild(td1);
	row.appendChild(td2);
	row.appendChild(td3);
	row.appendChild(td4);
	tbody.appendChild(row);

	taskCounter -= 1;
}
function delTask(el) {

	var row = el;

	while (row.tagName != "TR") {
		row = row.parentNode;
	}
	row.parentNode.removeChild(row);
	return false;
}
