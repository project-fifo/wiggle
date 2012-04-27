var ui = new Object();

!function ($) {
    var rfb;
    var center=$("#center");
    var machine_details = $(
	"<div class='row-fluid'>" +
	    "<div class='span3'><h3>Machine Details</h3></div>" +
	    "<div class='span9'>" +
	    "<div class='btn-group' style='float: right'>" + 
	    "<button class='btn btn-success' id='machine-detail-start' disabled='true'>Start</button>" +
	    "<button class='btn btn-warning' id='machine-detail-reboot' disabled='true'>Reboot</button>" +
	    "<button class='btn btn-danger' id='machine-detail-stop' disabled='true'>Stop</button>" +
	    "</div>" +
	    "</div>" +
	    "<div class='span2'>ID</div><div class='span9' id='machine-detail-id'>-</div>" +
	    "<div class='span2'>Name</div><div class='span9' id='machine-detail-name'>-</div>" +
	    "<div class='span2'>Type</div><div class='span9' id='machine-detail-type'>-</div>" +
	    "<div class='span2'>State</div><div class='span9' id='machine-detail-state'>-</div>" +
	    "<div class='span2'>Memory</div><div class='span9' id='machine-detail-memory'>-</div>" +
	    "<div class='span2'>IPs</div><div class='span9' id='machine-detail-ips'>-</div>" +
	    "<div class='span2'>Dataset</div><div class='span9' id='machine-detail-dataset'>-</div>" +
	    "<div class='span2'>Created</div><div class='span9' id='machine-detail-created'>-</div>" +
	    "</div>");
    var vnc_view = $(
	'<ul class="nav nav-tabs" id="detail-tabs">' +
	    '<li class="active"><a href="#tab-details" id="details-btn" data-toggle="tab">Details</a></li>' +
	    '<li><a href="#tab-vnc" id="vnc-btn" data-toggle="tab">vnc</a></li>' +
	    '</ul>' +
	    '<div class="tab-content">' +
	    '<div class="tab-pane active" id="tab-details"></div>' +
	    '<div class="tab-pane" id="tab-vnc">' +
	    '<div id="noVNC_screen">' +
	    '<div id="noVNC_status_bar" class="noVNC_status_bar" style="margin-top: 0px;">' +
	    '<table border=0 width="100%"><tr>' +
	    '<td><div id="noVNC_status">Loading</div></td>' +
	    '<td width="1%"><div id="noVNC_buttons">' +
	    '<input type=button value="Send CtrlAltDel" id="sendCtrlAltDelButton">' +
	    '</div></td>' +
	    '</tr></table>' +
	    '</div>' +
	    '<canvas id="noVNC_canvas" width="640px" height="20px">' +
	    'Canvas not supported.' +
	    '</canvas>' +
	    '</div>' +
	    '</div>' +
	    '</div>');
    
    var machine_form = $(
	"<div>" +
	    "<label>Name</label><input type='text' id='machine-new-name'/></br>" +
	    "<label>Package</label><select id='machine-new-package' name='package'/></br>" +
	    "<label>Dataset</label><select id='machine-new-dataset' name='dataset'/></br>" +
	    "<button class='btn btn-success' id='machine-new-btn'>Create</button>" +
	"</div>");

    function delete_vm() {
	var id=$(".machine.active").data("id");
	var go = $('<a href="#" class="btn btn-danger" data-dismiss="modal">Delete!</a>');
	var cancle = $('<a href="#" class="btn" data-dismiss="modal">Cancle</a>');
	
	var modal = $("#modal");
	var btns =  $("#modal .modal-footer");
	$("#modal .modal-header h3").text("Delete VM");
	$("#modal .modal-body p").
	    append("You are about to delete the VM ").
	    append($("<b>" +$(".machine.active").val() + "</b>")).
	    append(", this action can not be reversed! all data willbe lost forever!");
	go.click(function(){
	    $.ajax({
		url: "/my/machines/"+id,
		type: 'DELETE',
		dataType: 'json',
		success: function () {
		    $("#" + id + "-menu").remove();
		    activate_machine($(".machine").first().data("id"));
		}
	    });
	});
	btns.empty().
	    append(cancle).
	    append(go);
	modal.modal();	
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
		    var new_vm = {};
		    new_vm.name = vm.zonename;
		    new_vm.id = vm.uuid;
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
	var keys = ["id", "name", "type", "state", "memory", "created", "dataset"];
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
    function disconnect_vnc() {
	if (rfb)
	    rfb.disconnect();
    }
    function show_machine(data) {
	var c = center;
	c.empty();
	disconnect_vnc();
	if (data.type == "kvm") {
	    //we have VNC
	    c.append(vnc_view);
	    $('#detail-tabs a:first').tab('show')
	    c = $("#tab-details");
	    $("#vnc-btn").click(function () {
		init_vnc(data.id)
	    });
	    $("#details-btn").click(disconnect_vnc);
	};
	c.append(machine_details);
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

    function update_state(data) {
	var state = $("#" + data.id + "-state");
	state.
	    removeClass("badge-success").
	    removeClass("badge-warning").
	    removeClass("badge-important");
	var disp_state = $("#machine-detail-state");

	if ($("#machine-detail-id").text() == data.id) {
	    update_machine(data);
	}
	if (data.state == "running") {
	    state.addClass("badge-success");
	} else if (data.state == "stopped") {
	    state.addClass("badge-error");
	} else {
	    state.addClass("badge-warning");
	}
	    
    };
    function add_machine(data, show) {
	var id = data.id;
	var name = data.name;
	var state = $("<span class='badge'></span>").
	    attr("id", id + "-state");
	var li = $("<li class='machine'></li>").
	    attr("id", id + "-menu").
	    append($("<a></a>").
		   append(state).
		   append("&nbsp;" + name)).
	    data("id", id).
	    click(machine_click_fn);
	$("#machines").after(li);
	update_state(data);
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
/*	ui.refresh = setInterval(function () {
	    $.ajax({
		url: "/my/machines",
		dataType: 'json',
		success: function (data) {
		    for (var i = 0; i < data.length; i++) {
			update_state(data[i]);
		    }
		}
	    });
	}, 1000);*/
    };

    function updateState(rfb, state, oldstate, msg) {
        var s, sb, cad, level;
        s = $D('noVNC_status');
        sb = $D('noVNC_status_bar');
        cad = $D('sendCtrlAltDelButton');
        switch (state) {
        case 'failed':       level = "error";  break;
        case 'fatal':        level = "error";  break;
        case 'normal':       level = "normal"; break;
        case 'disconnected': level = "normal"; break;
        case 'loaded':       level = "normal"; break;
        default:             level = "warn";   break;
        }
	
        if (state === "normal" && cad) { cad.disabled = false; }
        else                    { if (cad) cad.disabled = true; }
	
        if (typeof(msg) !== 'undefined' && sb && s) {
            sb.setAttribute("class", "noVNC_status_" + level);
            s.innerHTML = msg;
        }
    }
    function sendCtrlAltDel() {
        rfb.sendCtrlAltDel();
        return false;
    }

    function init_vnc(id) {
        var host, port, password, path, token;
	
        $D('sendCtrlAltDelButton').style.display = "inline";
        $D('sendCtrlAltDelButton').onclick = sendCtrlAltDel;

        // By default, use the host and port of server that served this file
        host = WebUtil.getQueryVar('host', window.location.hostname);
        port = WebUtil.getQueryVar('port', window.location.port);
	
        path = "machines/" + id + "/vnc"
        rfb = new RFB({'target':       $D('noVNC_canvas'),
                       'encrypt':      WebUtil.getQueryVar('encrypt',
							   (window.location.protocol === "https:")),
                       'true_color':   WebUtil.getQueryVar('true_color', true),
                       'local_cursor': WebUtil.getQueryVar('cursor', true),
                       'shared':       WebUtil.getQueryVar('shared', true),
                       'view_only':    false,
                       'updateState':  updateState});
	rfb.connect(host, port, password, path);
    };
}(window.jQuery);


