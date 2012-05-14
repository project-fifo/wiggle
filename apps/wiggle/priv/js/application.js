var ui = new Object();

!function ($) {
    var rfb;
    var center=$("#center");

    function delete_vm() {
	var id=$(".machine.active").data("id");
	var go = $('<a href="#" class="btn btn-danger" data-dismiss="modal">Delete!</a>');
	var cancle = $('<a href="#" class="btn" data-dismiss="modal">Cancle</a>');
	
	var modal = $("#modal");
	var btns =  $("#modal .modal-footer");
	$("#modal .modal-header h3").text("Delete VM");
	$("#modal .modal-body p").
	    empty().
	    append("You are about to delete the VM ").
	    append($("<b>" +$(".machine.active").data("id") + "</b>")).
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
	if (data.ips == "") 
	    data.ips = "-"
	data = extend_machine_data(data);
	c.append(ich.details(data))
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
	update_machine(data);
	
    };
    function click_package(e, i) {
	var id = $(this).data("id");
	
	var obj = $(this).data("obj");
	if (obj) {
	    show_package(obj);
	};
    }
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

    function update_state(data) {
	var state = $("#" + data.id + "-state");
	state.attr("class","badge");

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
	ui.refresh = setInterval(function () {
	    return;
	    $.getJSON("/my/machines", function (data) {
		for (var i = 0; i < data.length; i++) {
		    var d = extend_machine_data(data[i]);
		    update_state(d);
		}
	    });
	}, 1000);
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
    
    load_templtes(["machine_details",
		   "details",
		   "package",
		   "machine_list_item",
		   "other_list_item",
		   "machine_form"]);
}(window.jQuery);
