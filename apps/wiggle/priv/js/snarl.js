var snarl = new Object();
var debug;
!function ($){
    var permissions = new Object();
    permissions.data = new Object();
    var selected;
    var vm_permissions = {
	"start": true,
	"stop": true,
	"reboot": true,
	"edit": true,
	"delete": true,
	"vnc": true,
	"info": true
    };
    var wiggle = {
	"login": true,
	"module": {
	    "about": true,
	    "account": true,
	    "admin": true,
	    "analytics": true,
	    "home": true,
	    "system": true,
	    "events": true
	}
    };
    var sniffle ={
	"info": true,
	"host": {
	    "add": {"Type":true}
	}
    };
    var group = {
	"add": true,
	"get": {
	    "Name": true
	},
	"UUID": {
	    "users": {
		"add":{
		    "UUID": true
		},
		"delete":{
		    "UUID": true
		}
	    },
	    "delete": true,
	    "grant": true,
	    "name": true,
	    "permissions": true,
	    "revoke": true
	}
    }
    var user = {
	"add": true,
	"passwd": true,
	"get": {
	    "Name": true
	},
	"UUID": {
	    "groups": {
		"add":{
		    "UUID": true
		},
		"delete":{
		    "UUID": true
		}
	    },
	    "delete": true,
	    "grant": true,
	    "name": true,
	    "permissions": true,
	    "revoke": true
	}
    };
    var permission = {
	"group": {
	    "grant": permissions.data,
	    "revoke": permissions.data
	},
	"user": {
	    "grant": permissions.data,
	    "revoke": permissions.data
	}
    };
    var option = {
	"Category": {
	    "list": true,
	    "delete": {
		"Name": true
	    },
	    "get": {
		"Name": true
	    },
	    "set": {
		"Name": true
	    }
	}
    };
    var dataset = {
	"create": true,	
	"Name": {
	    "get": true,
	    "delete": true
	}
    }
    var pkg = {
	"create": true,	
	"Name": {
	    "get": true,
	    "delete": true
	}
    }
    var key = {
	"create": true,	
	"Name": {
	    "get": true,
	    "delete": true
	}
    }
    var network = {
	"create": true,
	"Name": {
	    "delete": true,
	    "get": true,
	    "next_ip": true,
	    "release_ip": {
		"IP": true
	    }
	}
    }
    permissions.data["permission"] = permission;
    permissions.data["option"] = option
    permissions.data["group"] = group;
    permissions.data["user"] = user;
    permissions.data["dataset"] = dataset;
    permissions.data["package"] = pkg;
    permissions.data["key"] = key;
    permissions.data["network"] = network;
    permissions.data["service"] = {
	"wiggle": wiggle,
	"sniffle": sniffle,
    };
    
    permissions.data["package"] = {
	"Name": {
	    "add": true,
	    "delete": true,
	    "edit": true,
	    "view": true
	}
    };
    permissions.data["vm"] = {
	"Name": vm_permissions,
	"create": true
    };
    permissions.get = function(ks, current) {
	if (current == undefined) {
	    return permissions.get(ks, permissions.data);
	}
	if (current == true) {
	    return undefined;
	}
	if (ks.length == 0) {
	    return current||undefined;
	}
	var key = ks.shift();
	if (key == "...")
	    return undefined;
	if (current[key])
	    return permissions.get(ks, current[key]);
	var cks = keys(current);
	var blanks = cks.filter(is_placeholder);
	var first = blanks.shift();
	if (first)
	    return permissions.get(ks, current[first]);
    }
    function make_perm_li(perm, show_delete) {
	var li = $("<li></li>");
	for (var j = 0; j < perm.length; j++) {
	    var part = perm[j];
	    if (j > 0) {
		li.append("->");
	    }
	    var name;	    
	    if (name = part.perm) {
		li.append($("<span class='perm'></span>").append(name));
	    } else if (name = part.placeholder) {
		li.append($("<span class='placeholder'></span>").append(name));
	    };
	};
	if (show_delete) {
	    var btn = $("<button class='btn-mini btn-danger'>revoke</button>").
		click(function () {
		    var type = selected.type;
		    var id = selected.id;
		    $.ajax({
			url: "/my/" + type + "/" + id + "/permissions",
			type: 'DELETE',
			dataType: 'json',
			data: {
			    perms: JSON.stringify(perm)
			},
			success: function (data) {
			    if (selected.type == "users") {
				show_user(data);
			    } else if (selected.type == "groups") {
				show_group(data);
			    }
			}
		    });
		});
	    li.append(btn);
	}
	return li;
    }
    function show_user(data) {
	var ps = $("#permissions");
	ps.empty();	
	for (var i = 0; i < data.length; i++) {
	    ps.append(make_perm_li(data[i], true));
	};
    }
    function get_user(user) {
	$.getJSON("/my/users/" + user + "/permissions", function(data) {
	    get_user_groups(user);
	    show_user(data);
	});
    };
    function add_user_group(groups, group) {
	var li = $("<li></li>");
	var btn = $("<button class='btn-mini btn-danger'>remove</button>");
	var h4 = $("<h4></h4>").text(group).
	    append(btn);
	btn.click(function (){
	    $.ajax({
		url: "/my/users/" + selected.id + "/groups/" + group,
		type: 'DELETE',
		success: function (new_group) {
		    li.remove();
		}
	    });
	});
	groups.append(
	    li.append(h4).
		append($("<ul></ul>").
		       attr("id", "permissions_" + group)));
	get_group(group, group);

    }
    function show_user_groups(data) {
	var groups = $("<ul></ul>")
	$("#user_groups").
	    empty().
	    append($("<h3>Groups<select id='group_to_add'></select><button id='add_group_btn' class='btn-mini btn-success'>+</button></h3>")).
	    append(groups);
	$("#add_group_btn").click(function() {
	    var group = $("#group_to_add").val();
	    if (group != "") {
		$.ajax({
		    url: "/my/users/" + selected.id + "/groups",
		    type: 'POST',
		    dataType: 'json',
		    data: {
			"group": group
		    },
		    success: function (new_group) {
			add_user_group(groups,new_group);
		    }
		});
	    }
	});
	$.getJSON("/my/groups", function(groups){
	    $("#group_to_add").
		empty().
		append($("<option></option>"));
	    for (var i=0; i < groups.length; i++) {
		$("#group_to_add").append(
		    $("<option></option>").
			append(groups[i]));
	    }
	});
	for (var i = 0; i < data.length; i++) {
	    add_user_group(groups,data[i]);
	};
    }
    function get_user_groups(user) {
	$.getJSON("/my/users/" + user + "/groups", show_user_groups);
    };
    function show_group(data, name) {
	var ps = $("#permissions");
	if (name)
	    ps = $("#permissions_" + name);
	else 
	    $("#user_groups").empty();
	ps.empty();	
	for (var i = 0; i < data.length; i++) {
	    ps.append(make_perm_li(data[i], !name))
	};
    };
    function get_group(group, name) {
	$.getJSON("/my/groups/" + group + "/permissions", function(data) {
	    show_group(data, name);
	});
    };
    function create_select(ks) {
	var select = $("<select><option/><option>...</option></select>");
	for (var i = 0; i < ks.length; i++) {
	    select.append($("<option></option>").text(ks[i]));
	}
	select.change(option_pick);
	return select;
    }
    function add_list_user(user) {
	var li=$("<li></li>").
	    text(user).
	    click(function() {
		selected = {
		    type: "users",
		    id: user
		};
		get_user(user);
	    });
	var btn = $("<button class='btn-mini btn-danger'>x</button>");
	li.append(btn);
	btn.click(function() {
	    li.unbind("click");
	    $.ajax({
		url: "/my/users/" + user,
		type: 'DELETE',
		success: function () {
		    li.remove();
		}
	    });
	});
	$("#users").append(li);
    };
    function list_users(data) {
	$("#users").empty();
	$("#user_groups").empty();
	for (var i = 0; i < data.length; i++) {
	    add_list_user(data[i]);
	};
    }
    snarl.show_users = function() {
	$.getJSON("/my/users", list_users);
    }
    function add_list_group(group) {
	var btn = $("<button class='btn-mini btn-danger'>x</button>");
	var li=$("<li></li>").
	    text(group).
	    click(function() {
		debug = this;
		selected = {
		    type: "groups",
		    id: group
		};
		get_group(group);

	    });
	li.append(btn);
	btn.click(function() {
	    li.unbind("click");
	    $.ajax({
		url: "/my/groups/" + group,
		type: 'DELETE',
		success: function (data) {
		    li.remove();
		}
	    });
	});
	$("#groups").append(li);
    }
    function list_groups(data) {
	$("#groups").empty();
	for (var i = 0; i < data.length; i++) {
	    add_list_group(data[i]);
	};
    }
    snarl.show_groups = function() {
	$.getJSON("/my/groups", list_groups);
    }
    
    function is_placeholder(s) {
	return s.match(/^[A-Z]/);
    }

    function option_pick() {
	var rest = $("#new_permission").children();
	var idx = 0
	for (var i = rest.index($(this))+1; i < rest.length; i++) {
	    $(rest[i]).remove();
	}
	var selects = $("#new_permission select");
	var path = [];
	for (var i = 0; i < selects.length; i++) {
	    path.push(selects.eq(i).val());
	}
	var last = selects.last();
	debug = path;
	var perms = permissions.get(path);
	debug = perms;
	if (perms) {
	    var ks = keys(perms);
	    var select = create_select(ks)
	    last.after(select);
	}
	if (is_placeholder(last.val())) {
	    selects.last().after($("<input/>").val(last.val()));
	}
	
    }
    function add_user() {
	var name = $("#user_name").val();
	var pass = $("#user_pass").val();
	$.ajax({
	    url: "/my/users",
	    type: 'POST',
	    dataType: 'json',
	    data: {
		"login": name,
		"pass": pass
	    },
	    success: function (data) {
		add_list_user(data);
	    }
	});
    };
    function add_group() {
	var name = $("#group_name").val();
	$.ajax({
	    url: "/my/groups",
	    type: 'POST',
	    dataType: 'json',
	    data: {
		"name": name
	    },
	    success: function (data) {
		add_list_group(data);
	    }
	});
    }
    function get_ip() {
	$.getJSON("/my/networks/admin", function(data) {
	    $("#ip_range").attr("value",data.network);
	    $("#gateway").attr("value",data.gateway);
	    $('#netmask > option[value="' + data.netmask + '"]').attr("selected", true);	    
	});
    }
    function set_network() {
	$.ajax({
	    url: "/my/networks/external",
	    type: 'POST',
	    dataType: 'json',
	    data: {
		"first": $("#ip_range").val(),
		"netmask": $("#netmask").val(),
		"gateway": $("#gateway").val()
	    },
	    success: function (data) {
	    }
	});

    }

    snarl.init = function() {
	var selects = $("#new_permission select");
	selects.remove();
	var ks = keys(permissions.get([]));
	var select = create_select(ks);
	select.change(option_pick);
	$("#new_permission").append(select);
	snarl.show_users();
	snarl.show_groups();
	get_ip();
	$("#add_permission").click(add_permission);
	$("#add_user").click(add_user);
	$("#add_group").click(add_group);
	$("#set_network").click(set_network);
    };

    function add_permission() {	
	if (selected) {
	    var type = selected.type;
	    var id = selected.id;
	    $.ajax({
		url: "/my/" + type + "/" + id + "/permissions",
		type: 'POST',
		dataType: 'json',
		data: {
		    perms: JSON.stringify(get_new_permission())
		},
		success: function (data) {
		    if (selected.type == "users") {
			show_user(data);
		    } else if (selected.type == "groups") {
			show_group(data);
		    }
		}
	    });
	}
    }

    function get_new_permission() {
	var perms = $("#new_permission").children().map(function(i,o) {return $(o).val()});
	var res = []
	var i = 0
	while (i < perms.length) {
	    var first = 
		{"perm": perms[i]};
	    i++;
	    if (is_placeholder(first.perm)) {
		first = {"placeholder": perms[i]};
		i++;
	    }
	    res.push(first);
	}
	var last = res.pop()
	if (last.value != "")
	    res.push(last);
	return res;
    }
}(window.jQuery);

function keys(o) {
    var ks=[];
    var excluded = o["__excluded_keys"] || [];
    for (var k in o) {
	if (excluded.indexOf(k) == -1) {
	    ks.push(k)
	};
    };
    return ks;
};
