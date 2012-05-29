var permissions = new Object();
var snarl = new Object();
var debug;
!function ($){
    permissions.data = new Object();
    var selected;
    var vm_permissions = [
	"start",
	"stop",
	"reboot",
	"edit",
	"delete",
	"vnc"
    ];
    var wiggle = {
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
    permissions.data["permission"] = permission;
    permissions.data["option"] = option
    permissions.data["group"] = group;
    permissions.data["user"] = user;
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
	"Name": vm_permissions
    };
    permissions.get = function(ks, current) {
	if (current == undefined) {
	    console.log("1 - no current");
	    return permissions.get(ks, permissions.data);
	}
	if (current == true) {
	    console.log(2);
	    return undefined;
	}
	if (ks.length == 0) {
	    console.log(3);
	    return current||undefined;
	}
	console.log(4, ks);
	var key = ks.shift();
	console.log(5, key);
	console.log(6, ks);
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
	    var btn = $("<button>(revoke)</button>").
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
	$.getJSON("/my/users/" + user + "/own_permissions", function(data) {
	    get_user_groups(user);
	    show_user(data);
	});
    };
    function show_user_groups(data) {
	var groups = $("<ul></ul>")
	$("#user_groups").
	    empty().
	    append($("<h3>Groups</h3>")).
	    append(groups);
	for (var i = 0; i < data.length; i++) {
	    groups.append(
		$("<li></li>").
		    append($("<h4></h4>").text(data[i])).
		    append($("<ul></ul>").
			   attr("id", "permissions_" + data[i])));
	    get_group(data[i], data[i]);
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
	$("#users").append(
	    $("<li></li>").
		text(user).
		click(function() {
		    get_user(user);
		    get_user_groups(user);
			    selected = {
				type: "users",
				id: user
			    };
		})
	);
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
    function list_groups(data) {
	var groups = $("#groups");
	groups.empty();
	for (var i = 0; i < data.length; i++) {

	    (function(d){
		groups.append(
		    $("<li></li>").
			text(d).
			click(function() {
			    selected = {
				type: "groups",
				id: d
			    };
			get_group(d);
			})
		);
	    })(data[i]);
	};
    }
    snarl.show_groups = function() {
	$.getJSON("/my/groups", list_groups);
    }
    
    function is_placeholder(s) {
	return s.match(/^[A-Z]/);
    }

    function option_pick() {
	console.log("option change");
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
	console.log("path", path);
	debug = path;
	var perms = permissions.get(path);
	console.log("perms", typeof(perms), perms);
	debug = perms;
	if (perms) {
	    var ks = keys(perms);
	    console.log("ks", ks);
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
		data
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
	$("#add_permission").click(add_permission);
	$("#add_user").click(add_user);
    };
    function add_permission() {	
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
