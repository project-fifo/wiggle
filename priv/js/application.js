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

    var machine_form = $(
	"<div>" +
	    "<label>Name</label><input type='text' id='machine-new-name'/></br>" +
	    "<label>Package</label><select id='machine-new-package' name='package'/></br>" +
	    "<label>Dataset</label><select id='machine-new-dataset' name='dataset'/></br>" +
	    "<button class='btn btn-success' id='machine-new-btn'>Create</button>" +
	"</div>");

    function delete_vm() {
	var id=$(".machine.active").data("id");
	$.ajax({
	    url: "/my/machines/"+id,
	    type: 'DELETE',
	    dataType: 'json',
	    success: function () {
		$("#" + id + "-menu").remove();
		activate_machine($(".machine").first().data("id"));
	    }
	});
	
    }
    function machine_add_fn() {
	var pkg = $("#machine-new-package").val();
	var dataset = $("#machine-new-dataset").val();
	var name = $("#machine-new-name").val();
	$.ajax({
	    url: "/my/machines",
	    type: 'POST',
	    dataType: 'json',
	    data:{
		"name": name,
		"package": pkg,
		"dataset": dataset
	    },
	    success: function (vm) {
		if (vm) {
		    var new_vm = {}
		    new_vm.name = vm.zonename
		    new_vm.id = vm.uuid
		    alert(JSON.stringify(new_vm));
		    add_machine(new_vm, true);
		}
	    }
	});
	
	
    }
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

	if (!data.ips.length){
	    o.append("-");
	}
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
    function activate_machine(id) {
	var navItem = $("#" + id + "-menu");
	if(!navItem.hasClass("active")) {
	    $("#navlist .active").removeClass("active");
	    navItem.addClass("active");
	}
	get_machine(id,show_machine);

    };
    function machine_click_fn() {
	activate_machine($(this).data("id"));

    };
    function get_machine(uuid, callback) {
	$.ajax({
	    url: "/my/machines/"+uuid,
	    dataType: 'json',
	    success: callback
	});
    };
    function add_machine(data, show) {
	var id = data.id;
	var name = data.name;
	var li = $("<li class='machine'></li>").
	    attr("id", id + "-menu").
	    append($("<a></a>").
		   append(name)).
	    data("id", id).
	    click(machine_click_fn);
	$("#machines").after(li);
	if (show) {
	    activate_machine(id)
	}
    }
    function get_machines() {
	$.ajax({
	    url: "/my/machines",
	    dataType: 'json',
	    success: function (data) {
		for (var i = 0; i < data.length; i++) {
		    add_machine(data[i], i==0);
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



    function view_add_vm() {
	var center = $("#center");
	center.empty();
	center.append(machine_form);
	var datasets = $("#machine-new-dataset");
	datasets.empty()
	$.ajax({
	    url: "/my/datasets",
	    dataType: 'json',
	    success: function (data) {
		for (var i = 0; i < data.length; i++) {
		    var d = data[i];
		    var option = $("<option></option>").
			attr("value", d.id).
			append(d.name + " v" + d.urn.split(":")[3]);
		    if (d.default)
			option.attr("selected", true);
		    datasets.append(option);
		};
	    }
	});
	var select = $("#machine-new-package");
	select.empty()
	$.ajax({
	    url: "/my/packages",
	    dataType: 'json',
	    success: function (data) {
		for (var i = 0; i < data.length; i++) {
		    var d = data[i];
		    var option = $("<option></option>").
			attr("value", d.name).
			append(d.name);
		    select.append(option);
		};
	    }
	});
	$("#machine-new-btn").click(machine_add_fn)
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
	$("#machines-nav-add").click(view_add_vm);
	$("#machines-nav-del").click(delete_vm);

    };
}(window.jQuery);
