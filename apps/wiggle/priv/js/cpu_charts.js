// Based on the demo: https://github.com/ollieglass/pusher-d3-demo
var mpstat = new Object();

!function ($) {
    mpstat.create = function(uuid, data) {
	var obj = new Object();
	obj.cpu_count = data.length;
	var empty = []
	for (var i = 0; i < obj.cpu_count; i++) {
	    empty.push({user: 0, system: 0});
	}
	obj.t = 1297110663; // start time (seconds since epoch)
	obj.data = d3.range(200).map(function(){ return { time: obj.t, values: empty}}); // starting dataset,
	
	obj.updateNextValue = function (data) {
	    var result = { time: ++(obj.t), values: data };
	    return result;
	}
	
	obj.update = function(data) {
	    obj.data.shift();
	    while (data.length < obj.cpu_count) {
		data.push({user: 0, system: 0})
            }
	    obj.data.push(obj.updateNextValue(data));
	    obj.redraw();
	};
	
	
	// ====================================
	// d3 - static
	// ====================================
	
	var w = 2;
	var h = 20;
	
	var x = d3.scale.linear()
	    .domain([0, 1])
	    .range([0, w]);
	
	var y = d3.scale.linear()
	    .domain([0, 100])
	    .rangeRound([0, h]);
	chart = [];
	for (var i = 0; i < obj.cpu_count; i++) {
	    chart[i] = d3.select($("div#" + uuid + "-mpstat")[0]).append("svg")
		.attr("class", "chart")
		.attr("width", w * obj.data.length - 1)
		.attr("height", h);
	    
	    chart[i].selectAll("rect")
		.data(obj.data)
		.enter()
		.append("user")
		.attr("x", function(d, i) { return x(i) - .5; })
		.attr("y", function(d) { return h - y(d.values[i].user) - .5; })
		.attr("width", w)
		.attr("height", function(d) { return y(d.values[i].user); })
		.attr("class", "user")
		.append("system")
		.attr("x", function(d, i) { return x(i) - .5; })
		.attr("y", function(d) { return h - y(d.values[i].system) - .5 - y(d.values[i].user); })
		.attr("width", w)
		.attr("height", function(d) { return y(d.values[i].system); })
		.attr("class", "system");
	    
	    chart[i].append("line")
		.attr("x1", 0)
		.attr("x2", w * obj.data.length)
		.attr("y1", h - .5)
		.attr("y2", h - .5)
		.style("stroke", "#000");
	}
	
	// ====================================
	// d3 - update the chart
	// ====================================
	
	obj.redraw = function() {
	    for (var i = 0; i < obj.cpu_count; i++) {
		
		var rect_user = chart[i].selectAll("rect.user")
		    .data(obj.data, function(d) { return d.time; });
		var rect_system = chart[i].selectAll("rect.system")
		    .data(obj.data, function(d) { return d.time; });
		
		rect_user.enter().insert("rect", "line")
		    .attr("y", function(d) { return h - y(d.values[i].user) - .5; })
		    .attr("width", w)
		    .attr("height", function(d) { return y(d.values[i].user); })
		    .attr("class", "user")
		    .attr("x", function(d, i) { return x(i) - .5; });
		rect_system.enter().insert("rect", "line")
		    .attr("y", function(d) { return h - y(d.values[i].system) - .5 - y(d.values[i].user); })
		    .attr("width", w)
		    .attr("height", function(d) { return y(d.values[i].system); })
		    .attr("class", "system")
		    .attr("x", function(d, i) { return x(i) - .5; });
		
		rect_user
		    .attr("color", "red")
		    .attr("x", function(d, i) { return x(i) - .5; })
		    .attr("y", function(d) { return h - y(d.values[i].user) - .5; })
		    .attr("height", function(d) { return y(d.values[i].user); });
		rect_system
		    .attr("x", function(d, i) { return x(i) - .5; })
		    .attr("y", function(d) { return h - y(d.values[i].system) - .5 - y(d.values[i].user); })
		    .attr("height", function(d) { return y(d.values[i].system); })
		    .attr("class", "system");
		
		rect_user.exit()
		    .remove();
		
		rect_system.exit()
		    .remove();
	    }
	}
	obj.update(data);
	return obj;
    }
}($);
