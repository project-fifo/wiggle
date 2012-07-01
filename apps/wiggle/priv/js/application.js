var ui = new Object();
var stats = new Object();

!function ($) {
    var ws;
    var rfb;
    var ws_problem = 0;
    var center=$("#center");

    function watcher_action(action, type, uuid) {
	try {
	    var json = JSON.stringify({"action": action, "type": type, "uuid": uuid});
	    console.log(json);
	    console.log(ws);
	    ws.send(json);
	} catch (e) {
	    console.log(e);
	    ws_problem++;
	    if (ws_problem == 100) {
		alert("Websocket died!");
	    }
	//   setInterval(function(){
	//	watcher_action(action, type, uuid);
	//    }, 200);
	}
    }	
    function watch_machine(id) {
	watcher_action("subscribe", "vm", id);
    };
    function watch_host(id) {
	watcher_action("subscribe", "host", id);
    };
    function unwatch_machine(id) {
	watcher_action("unsubscribe", "vm", id);
    };

    function unwatch_machine(id) {
	watcher_action("unsubscribe", "host", id);
    };



    function delete_vm() {
	var id=$(".machine.active").data("id");
	var go = $('<a href="#" class="btn btn-danger" data-dismiss="modal">Delete!</a>');
	var cancle = $('<a href="#" class="btn" data-dismiss="modal">Cancle</a>');
	var name = $(".machine.active a").text().replace(/^\s*/,"");
	var modal = $("#modal");
	var btns =  $("#modal .modal-footer");
	$("#modal .modal-header h3").text("Delete VM");
	$("#modal .modal-body p").
	    empty().
	    append("You are about to delete the VM ").
	    append($("<b>" + name + "</b>")).
	    append("(" + $(".machine.active").data("id") + ")").
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

    function activate(navItem) {
	if(!navItem.hasClass("active")) {
	    $("#navlist .active").removeClass("active");
	    navItem.addClass("active");
	}
    }
    
    function extend_machine_data(data) {
	if (data.type == "kvm")
	    data.kvm = true;
	else
	    data.zone = true;
	data[data.state] = true;
	return data;
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
		    add_machine(vm, true);
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


    function update_detail_buttons(id, state) {
	$('#machine-detail-stop').
	    attr("disabled", true);
	$('#machine-detail-reboot').
	    attr("disabled", true);
	$('#machine-detail-start').
	    attr("disabled", true);
	if (state == "running") {
	    $('#machine-detail-stop').
		attr("disabled", false).
		click(function (){
		    machine_action(id, "stop", update_machine);
		});
	    $('#machine-detail-reboot').
		attr("disabled", false).
		click(function (){
		    machine_action(id, "reboot", update_machine);
		});
	} else if (state == "stopped") {
	    $('#machine-detail-start').
		attr("disabled", false).
		click(function (){
		    machine_action(id, "start", update_machine);
		});
	}

    }
    function update_machine(data) {
	var c = center;
	c.empty();
	var new_ips = "";
	if (data.ips) {
	    for (var i = 0; i < data.ips.length; i++) {
		if (i > 0 )
		    new_ips = new_ips + ", ";
		new_ips = new_ips + data.ips[i];
	    }
	}
	data.ips = new_ips;
	if (data.ips == "") 
	    data.ips = "-"
	var data = extend_machine_data(data);
	c.append(ich.details(data));
	var details = $("#detail-tabs");
	details.data("id", data.id);
	if (data.kvm) {
	    $.getJSON("/my/images", function (images) {
		var select = $("#boot-image").
		    empty().
		    append($("<option></option>"));
		for (var i=0; i < images.length; i++) {
		    select.append($("<option/>").append(images[i]));
		}
	    });
	}
	update_detail_buttons(data.id, data.state);
    }
    function show_machine(data) {	
	update_machine(data);
	
    };

    function click_package(e, i) {
	var id = $(this).data("id");
	
	var obj = $(this).data("obj");
	if (obj) {
	    show_package(obj);
	};
    };

    function show_package(data) {
	center.empty();
	center.append(ich.package(data));
	var navItem = $("#" + data.name + "-menu");
	activate(navItem);
	$('#packageForm').
	    submit(function(f) {
		var obj = {
		    name: $("#packageName").val(),
		    memory: $("#packageRam").val(),
		    disk: $("#packageDisk").val(),
		    swap: $("#packageSwap").val()
		};
		$.ajax({
		    url: "/my/packages",
		    type: 'POST',
		    dataType: 'json',
		    data: obj,
		    success: function (pkg) {
			activate(add_other("packages", pkg, pkg.name, pkg.name, click_package));
		    }});
		return false;
	    }).
	    data("id", data.id);
    }
    function activate_machine(id) {
	var navItem = $("#" + id + "-menu");
	activate(navItem);
	get_machine(id,show_machine);

    };

    function machine_click_fn() {
	activate_machine($(this).data("id"));

    };
    function get_machine(uuid, callback) {
	$.getJSON("/my/machines/"+uuid, callback);
    };

    function update_state(uuid, state) {
	

	var details = $("#detail-tabs");
	if (details.data("id") == uuid) {
	    update_detail_buttons(uuid, state);	    
	    $("#machine-detail-state").text(state);
	};
	
	var s = $("#" + uuid + "-state");
	s.attr("class","badge");

	if (state == "running")
	    s.addClass("badge-success");
	else if (state == "stopped")
	    s.addClass("badge-error");
	else
	    s.addClass("badge-warning");
    };

    function add_machine(data, show) {
	watch_machine(data.id);
	var li = ich.machine_list_item(data).
	    data("id", data.id).
	    click(machine_click_fn);
	$("#machines").after(li);
	update_state(data.id, data.state);
	if (show)
	    activate_machine(data.id)
    }

    function get_machines() {
	$.getJSON("/my/machines", function (data) {
	    for (var i = 0; i < data.length; i++) {
		d = extend_machine_data(data[i]);
		add_machine(d, i==0);
	    }
	});
    };

    function add_other(kind, obj, id, name, click_fn) {
	var parent = $("#" + kind);
	var old = $("#" + id + "-menu");
	var li = ich.other_list_item({"id": id,
				      "name": name,
				      "type": kind}).
	    data("obj", obj).
	    data("id", id);
		if (click_fn) {
	    li.click(click_fn);
	}
	if (old.length) {
	    li.attr("class", old.attr("class"));
	    old.replaceWith(li);
	} else {
	    parent.after(li);
	}
	return li;
	
    };
    
    function get_other(name, fmtr, idkey, click_fn) {
	var f = fmtr || function (data) { return data.name; };
	var k = idkey || "id";
	$.getJSON("/my/" + name, function (data) {
	    for (var i = 0; i < data.length; i++) {
		var obj = data[i];
		add_other(name, obj, obj[k], f(obj), click_fn)
	    };
	});
    };


    function view_add_pkg() {
	show_package({});
    };

    function delete_pkg() {
	var pkg = $(".packages.active");
	x = pkg;
	if (pkg.length) {
	    var id = pkg.data("id");
	    $.ajax({
		url: "/my/packages/"+id,
		type: 'DELETE',
		dataType: 'json',
		success: function () {
		    $("#" + id + "-menu").remove();
		    show_package($(".packages").first().data("obj"));
		}
	    });
	}
    };

    function view_add_vm() {
	center.empty();
	center.append(ich.machine_form());
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
	init_event_socket();
	get_other("packages", false, "name", click_package);
	get_other("datasets",
		  function (data) {
		      return data.name +
			  " v" + 
			  data.urn.split(":")[3];
		  });

	$("#packages-nav-add").click(view_add_pkg);
	$("#packages-nav-del").click(delete_pkg);
	
	$("#machines-nav-add").click(view_add_vm);
	$("#machines-nav-del").click(delete_vm);
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
    function load_templtes(tpls) {
	for (var i = 0; i < tpls.length; i++) {
	    load_template(tpls[i]);
	}
    }
    
    function init_event_socket(initfn){
	if ("MozWebSocket" in window) {
	    WebSocket = MozWebSocket;
	}
	if (("WebSocket" in window) && (ws_problem < 30)) {
            host = window.location.hostname;
            port = window.location.port;
	                if (port == "")
              port = "80";

	    // browser supports websockets
	    console.log("ws://" + host + ":" + port + "/events");
	    ws = new WebSocket("ws://" + host + ":" + port + "/events");
	    ws.onopen = function() {
		// websocket is connected
		ws_problem = 0;
		console.log("ws://" + host + ":" + port + "/events - opened");
		initfn();
	    };
	    ws.onmessage = function (evt) {
		console.log(evt);
		var receivedMsg = evt.data;
		var json = JSON.parse(receivedMsg);
		
		switch (json.event) {
		    case "stat":
		    update_host_stats(json.uuid, json.stats);
		    case "state change":
		    console.log(receivedMsg);
		    update_state(json.uuid, json.state);
		    break;
		}
	    };
	    ws.onclose = function() {
		// websocket was closed
		ws_problem++;
		if (ws_problem <= 30) {
		    setTimeout(init_event_socket,1000*ws_problem);
		} else {
		    alert("Sorry, giving up on the event socket - something is wrong.");
		}
	    };
	} else {
	    // browser does not support websockets
	    addStatus("sorry, your browser does not support websockets.");
	}
    }

    function update_host_stats(host, stats) {
	var gauge_mem = $("#" + host+'-memory').data("gauge");
	gauge_mem.config.maxValue = stats.memory.total/(1024* 1024);
	gauge_mem.draw()
	gauge_mem.setValue((stats.memory.total - stats.memory.free)/(1024*1024));
	var gauge_user = $("#" + host+'-user').data("gauge");
	gauge_user.setValue(stats.cpu.user);
	var gauge_system = $("#" + host+'-system').data("gauge");
	gauge_system.setValue(stats.cpu.system);
	var gauge_ioblock = $("#" + host+'-ioblock').data("gauge");
	gauge_ioblock.setValue(stats.kthr.blocked);
	$("#" + host+'-pgin').data("gauge").setValue(stats.page.in);
	$("#" + host+'-pgout').data("gauge").setValue(stats.page.out);


    }
    function add_stat_host(host) {
	$("#hosts").append(ich.host({uuid: host}));
	var green = "#8FFEDD";
	var yellow = "#FFFF99";
	var red = "#FF8A8A";
	var green = "#eee";
	var yellow = "#ccc";
	var red = "#999";

	var gauge_mem = new Gauge({ 
	    renderTo: host+'-memory',
	    width: 150,
	    height: 150,
	    highlights: [{ from: 20, to: 60, color: green }, 
			 { from: 60, to: 80, color: yellow }, 
			 { from: 80, to: 100, color: red}],
	    valueFormat:{"int": 3, "dec": 0}
	});
	$("#" + host+'-memory').data("gauge",gauge_mem);
	gauge_mem.draw();

	var gauge_user = new Gauge({ 
	    renderTo: host+'-user',
	    width: 150,
	    height: 150,
	    highlights: [{ from: 0, to: 30, color: green}, 
			 { from: 30, to: 80, color: yellow}, 
			 { from: 80, to: 100, color: red}]
	});
 	$("#" + host+'-user').data("gauge",gauge_user);
	gauge_user.draw();

	var gauge_system = new Gauge({
	    renderTo: host+'-system',
	    width: 150,
	    height: 150,
	    highlights: [{ from: 0, to: 30, color: green}, 
			 { from: 30, to: 80, color: yellow}, 
			 { from: 80, to: 100, color: red}]
	});
	$("#" + host+'-system').data("gauge",gauge_system);
	gauge_system.draw();
	var gauge_ioblock = new Gauge({
	    renderTo: host+'-ioblock',
	    width: 150,
	    height: 150,
	    highlights: [{ from: 0, to: 10, color: green}, 
			 { from: 10, to: 50, color: yellow}, 
			 { from: 50, to: 100, color: red}]
	}); 
	$("#" + host+'-ioblock').data("gauge",gauge_ioblock);
	gauge_ioblock.draw();

	var gauge_pgin = new Gauge({
	    renderTo: host+'-pgin',
	    width: 150,
	    height: 150,
	    highlights: [{ from: 0, to: 10, color: green}, 
			 { from: 10, to: 50, color: yellow}, 
			 { from: 50, to: 100, color: red}]
	}); 
	$("#" + host+'-pgin').data("gauge",gauge_pgin);
	gauge_pgin.draw();
	var gauge_pgout = new Gauge({
	    renderTo: host+'-pgout',
	    width: 150,
	    height: 150,
	    highlights: [{ from: 0, to: 10, color: green}, 
			 { from: 10, to: 50, color: yellow}, 
			 { from: 50, to: 100, color: red}]
	}); 
	$("#" + host+'-pgout').data("gauge",gauge_pgout);
	gauge_pgout.draw();



    }
    function stat_hosts() {
	$.getJSON("/my/hosts", function (data) {
	    for (var i = 0; i < data.length; i++) {
		add_stat_host(data[i]);
		watch_host(data[i]);
	    };
	});

    }
    
    stats.init = function() {
	init_event_socket(function(){
	    stat_hosts();
	});
    }
    
    load_templtes(["machine_details",
		   "details",
		   "package",
		   "machine_list_item",
		   "other_list_item",
		   "machine_form",
		   "host"]);
}(window.jQuery);

