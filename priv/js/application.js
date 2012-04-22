var ui = new Object();

!function ($) {
    var center=$("#center");
    var machine_details = $(
	    "<div class='row'>" +
	    "<div class='span2'>ID</div><div class='span8' id='machine-detail-id'>-</div>" +
	    "<div class='span2'>Name</div><div class='span8' id='machine-detail-name'>-</div>" +
	    "<div class='span2'>Type</div><div class='span8' id='machine-detail-type'>-</div>" +
	    "<div class='span2'>State</div><div class='span8' id='machine-detail-state'>-</div>" +
	    "<div class='span2'>Memory</div><div class='span8' id='machine-detail-memory'>-</div>" +
	    "<div class='span2'>IPs</div><div class='span8' id='machine-detail-ips'>-</div>" +
	    "<div class='span2'>Created</div><div class='span8' id='machine-detail-create'>-</div>" +
	    "<div class='span10'>" +
	    "<div class='btn-group' style='float: right'>" + 
	    "<button class='btn btn-success' id='machine-detail-start' disabled='true'>Start</button>" +
	    "<button class='btn btn-warning' id='machine-detail-reboot' disabled='true'>Reboot</button>" +
	    "<button class='btn btn-danger' id='machine-detail-stop' disabled='true'>Stop</button>" +
	    "</div>" +
	    "</div>" +
	    "</div>");


    function machine_action(uuid, action, callback) {
	$.ajax({
	    url: "/my/machines/"+uuid +"?action=" + action,
	    type: 'POST',
	    dataType: 'json',
	    success: callback
	});

    }
    function update_machine(data) {
	var keys = ["id", "name", "type", "state", "memory", "create"];
	for (var i = 0; i < keys.length; i++) {
	    var key = keys[i];
	    var o = $('#machine-detail-' + key);
	    o.empty();
	    o.append(data[key]);
	};
	var o = $('#machine-detail-ips');
	o.empty();
	for (var i = 0; i < data.ips.length; i++) {
	    if (i > 0 )
		o.append(", ");
	    o.append(data.ips[i]);
	}

	$('#machine-detail-stop').
	    attr("disabled", true);
	$('#machine-detail-reboot').
	    attr("disabled", true);
	$('#machine-detail-start').
	    attr("disabled", true);
	if (data.state == "running") {
	    $('#machine-detail-stop').
		attr("disabled", false).
		click(function (){
		    machine_action(data.id, "stop", update_machine);
		});
	    $('#machine-detail-reboot').
		attr("disabled", false).
		click(function (){
		    machine_action(data.id, "reboot", update_machine);
		});
	} else if (data.state == "stopped") {
	    $('#machine-detail-start').
		attr("disabled", false).
		click(function (){
		    machine_action(data.id, "start", update_machine);
		});;
	}
    }
    function show_machine(data) {
	center.empty();
	center.append(machine_details);
	    
	update_machine(data);
	$('#machine-detail-start').click(function (){machine_action(data.id, "start")});
	$('#machine-detail-reboot').click(function (){machine_action(data.id, "reboot")});
    };
    function activate_machien(id) {
	var navItem = $("#" + id + "-menu");
	if(!navItem.hasClass("active")) {
	    $("#navlist .active").removeClass("active");
	    navItem.addClass("active");
	}
	get_machine(id,show_machine);

    };
    function machine_click_fn() {
	activate_machien($(this).data("id"));

    };
    function get_machine(uuid, callback) {
	$.ajax({
	    url: "/my/machines/"+uuid,
	    dataType: 'json',
	    success: callback
	});
    };
    function get_machines() {
	$.ajax({
	    url: "/my/machines",
	    dataType: 'json',
	    success: function (data) {
		for (var i = 0; i < data.length; i++) {
		    var id = data[i].id;
		    var name = data[i].name;
		    var li = $("<li></li>").
			attr("id", id + "-menu").
			append($("<a></a>").
			       append(name)).
			data("id", id).
			click(machine_click_fn);
		    if (i == 0) {
			li.addClass("active");
			show_machine(data[i]);
		    }
		    $("#packages").before(li);
		}
	    }
	});
    };
    function get_other(name, fmtr, click_fn) {
	var parent = $("#" + name);
	var f = fmtr || function (data) { return data.name; };
	$.ajax({
	    url: "/my/" + name,
	    dataType: 'json',
	    success: function (data) {
		for (var i = 0; i < data.length; i++) {
		    var id = data[i].id;
		    var name = data[i].name;
		    var li = $("<li></li>").
			attr("id", id + "-menu").
			append($("<a></a>").
			       append(f(data[i]))).
			data("id", id);
		    if (click_fn) {
			click(click_fn);
		    }
		    parent.after(li);
		};
	    }
	});
    };
    ui.init = function () {
	get_machines();
	get_other("packages");
	get_other("datasets",
		  function (data) {
		      return data.name +
			  " v" + 
			  data.urn.split(":")[3];
		  });
    };
}(window.jQuery);
