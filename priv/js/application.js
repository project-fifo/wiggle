var ui = new Object();

!function ($) {
    var rfb;
    var center=$("#center");
    
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
	var data = {"action": action};
	var image = $("#boot-image").val();
	if (image != "") {
	    data.image = image;
	}
	$.ajax({
	    url: "/my/machines/"+uuid +"?action=" + action,
	    type: 'POST',
	    dataType: 'json',
	    data: data,
	    success: callback
	});

    }
    function update_machine(data) {
	var c = center;
	c.empty();
	var new_ips = "";
	for (var i = 0; i < data.ips.length; i++) {
	    if (i > 0 )
		new_ips = new_ips + ", ";
	    new_ips = new_ips + data.ips[i];
	}
	data.ips = new_ips;
	if (data.type == "kvm")
	    data.kvm = true;
	else
	    data.zone = true;
	    
	c.append(ich.details(data))
	if (data.kvm) {
	    $("#vnc-btn").click(function () {
		init_vnc(data.id);
	    });
	    $.getJSON("/my/images", function (images) {
		var select = $("#boot-image").
		    empty().
		    append($("<option></option>"));
		for (var i=0; i < images.length; i++) {
		    select.append($("<option/>").append(images[i]));
		}
	    });
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
	disconnect_vnc();
	update_machine(data);
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
	$.getJSON("/my/machines/"+uuid, callback);
    };

    function update_state(data) {
	var state = $("#" + data.id + "-state");
	state.attr("class","badge");

	if ($("#machine-detail-id").text() == data.id)
	    update_machine(data);

	if (data.state == "running")
	    state.addClass("badge-success");
	else if (data.state == "stopped")
	    state.addClass("badge-error");
	else
	    state.addClass("badge-warning");
	    
    };

    function add_machine(data, show) {
	var li = ich.machine_list_item(data).
	    data("id", data.id).
	    click(machine_click_fn);
	$("#machines").after(li);
	update_state(data);
	if (show)
	    activate_machine(data.id)
    }

    function get_machines() {
	$.getJSON("/my/machines", function (data) {
	    for (var i = 0; i < data.length; i++) {
		add_machine(data[i], i==0);
	    }
	});
    };

    
    function get_other(name, fmtr, click_fn) {
	var parent = $("#" + name);
	var f = fmtr || function (data) { return data.name; };
	$.getJSON("/my/" + name, function (data) {
	    for (var i = 0; i < data.length; i++) {
		var li = ich.other_list_item({"id": data[i].id,
					      "name": f(data[i])}).
		    data("id", data[i].id);
		if (click_fn) {
		    li.click(click_fn);
			  }
		parent.after(li);
	    };
	});
    };


    function view_add_vm() {
	center.empty();
	center.append(machine_form);
	var datasets = $("#machine-new-dataset");
	datasets.empty();
	$.getJSON("/my/datasets", function (data) {
	    for (var i = 0; i < data.length; i++) {
		var d = data[i];
		var option = $("<option></option>").
		    attr("value", d.id).
		    append(d.name + " v" + d.urn.split(":")[3]);
		if (d.default)
		    option.attr("selected", true);
		datasets.append(option);
		
	    };
	});
	
	var select = $("#machine-new-package");
	select.empty();
	select.append($("<option></option>"));
	$.getJSON("/my/packages", function (data) {
	    for (var i = 0; i < data.length; i++) {
		var d = data[i];
		var option = $("<option></option>").
		    attr("value", d.name).
		    append(d.name).data("pkg", d);
		select.append(option);
	    };
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
	ui.refresh = setInterval(function () {
	    $.getJSON("/my/machines", function (data) {
		for (var i = 0; i < data.length; i++) {
		    update_state(data[i]);
		}
	    });
	}, 1000);
    };
    
    function updateState(rfb, state, oldstate, msg) {
        var s, sb, cad, level;
        s = $D('noVNC_status');
        sb = $D('noVNC_status_bar');
        cad = $D('sendCtrlAltDelButton');
        switch (state) {
        case 'failed':       level = "error";  break;
        case 'fatal':        level = "error";  break;
        case 'normal':       level = "info"; break;
        case 'disconnected': level = "info"; break;
        case 'loaded':       level = "info"; break;
        default:             level = "warn";   break;
        }
	
        if (state === "normal" && cad) { cad.disabled = false; }
        else                    { if (cad) cad.disabled = true; }
	
        if (typeof(msg) !== 'undefined' && sb && s) {
            s.setAttribute("class", "span9 alert alert-" + level);
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
        host = window.location.hostname;
        port = window.location.port;
	if (port == "")
	    port = 80;
	
        path = "machines/" + id + "/vnc"
        rfb = new RFB({'target':       $D('noVNC_canvas'),
                       'encrypt':      (window.location.protocol === "https:"),
                       'true_color':   true,
                       'local_cursor': true,
                       'shared':       true,
                       'view_only':    false,
                       'updateState':  updateState});
	rfb.connect(host, port, password, path);
    };

    function load_template(id) {
	$.ajax({
	    url: "/tpl/" + id + ".html",
	    dataType: 'text',
	    success: function (data) {
		ich.addTemplate(id, data);
	    }
	});
    }
    function load_partial(id) {
	$.ajax({
	    url: "/tpl/" + id + ".html",
	    dataType: 'text',
	    success: function (data) {
		ich.addPartial(id, data);
	    }
	});
    }
    
    load_template("machine_details");
    load_template("details");
    $.getJSON('/tpl/main.json', function (templates) {
	$.each(templates, function (name) {
	    var template = templates[name];
	    ich.addTemplate(name, template);
	});
    });
}(window.jQuery);


