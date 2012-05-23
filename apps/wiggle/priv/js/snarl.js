var permissions = new Object();
var snarl = new Object();
!function ($){
    permissions.data = new Object();
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
	if (current == undefined)
	    return permissions.get(ks, permissions.data);
	if (current == true) 
	    return [];
	if (ks.length == 0)
	    return current||[];
	var key = ks.shift();
	if (key == "...")
	    return [];
	if (current[key])
	    return permissions.get(ks, current[key]);
	var cks = keys(current);
	var blanks = cks.filter(function (x) { return x.match(/^[A-Z]/) });
	var first = blanks.shift();
	if (first)
	    return permissions.get(ks, current[first]);
    }

    snarl.user_permissions = function(user) {
	$.getJSON("/my/users/" + user + "/permissions", function (data) {
	    for (var i = 0; i < data.length; i++) {
	    };
	});
    };
    snarl.show_users = function() {
	$.getJSON("/my/users", function (data) {
	    var users = $("#users");
	    users.empty();
	    for (var i = 0; i < data.length; i++) {
		var d = data[i];
		users.append(
		    $("<li></li>").
			text(d).
			click(function() {
			    snarl.user_permissions(d);
			})
		);
	    };
	});

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
