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
//	    d3.select('div.count p span.current').html(value);
	    return result;
	}
	
	obj.update = function(data) {
	    obj.data.shift();
	    obj.data.push(obj.updateNextValue(data));
	    obj.redraw();
	};
	
	
	// ====================================
	// d3 - static
	// ====================================
	
	obj.w = 2;
	obj.h = 50;
	
	obj.x = d3.scale.linear()
	    .domain([0, 1])
	    .range([0, obj.w]);
	
	obj.y = d3.scale.linear()
	    .domain([0, 100])
	    .rangeRound([0, obj.h]);
	obj.chart = [];
	for (var i = 0; i < obj.cpu_count; i++) {
	    obj.chart[i] = d3.select($("div#" + uuid + "-mpstat")[0]).append("svg")
		.attr("class", "chart")
		.attr("width", obj.w * obj.data.length - 1)
		.attr("height", obj.h);
	    
	    obj.chart[i].selectAll("rect")
		.data(obj.data)
		.enter()
		.append("user")
		.attr("x", function(d, i) { return obj.x(i) - .5; })
		.attr("y", function(d) { return obj.h - obj.y(d.values[i].user) - .5; })
		.attr("width", obj.w)
		.attr("height", function(d) { return obj.y(d.values[i].user); })
		.attr("class", "user")
		.append("system")
		.attr("x", function(d, i) { return obj.x(i) - .5; })
		.attr("y", function(d) { return obj.h - obj.y(d.values[i].system) - .5 - obj.y(d.values[i].user); })
		.attr("width", obj.w)
		.attr("height", function(d) { return obj.y(d.values[i].system); })
		.attr("class", "system");
	    
	    obj.chart[i].append("line")
		.attr("x1", 0)
		.attr("x2", obj.w * obj.data.length)
		.attr("y1", obj.h - .5)
		.attr("y2", obj.h - .5)
		.style("stroke", "#000");
	}

// ====================================
// d3 - update the chart
// ====================================

	obj.redraw = function() {
	    for (var i = 0; i < obj.cpu_count; i++) {
		
		var rect_user = obj.chart[i].selectAll("rect.user")
		    .data(obj.data, function(d) { return d.time; });
		var rect_system = obj.chart[i].selectAll("rect.system")
		    .data(obj.data, function(d) { return d.time; });
	    
		rect_user.enter().insert("rect", "line")
		    .attr("x", function(d, i) { return obj.x(i + 1) - .5; })
		    .attr("y", function(d) { return obj.h - obj.y(d.values[i].user) - .5; })
		    .attr("width", obj.w)
		    .attr("height", function(d) { return obj.y(d.values[i].user); })
		    .attr("class", "user")
		    .transition()
		    .duration(0)
		    .attr("x", function(d, i) { return obj.x(i) - .5; });
		rect_system.enter().insert("rect", "line")
		    .attr("x", function(d, i) { return obj.x(i + 1) - .5; })
		    .attr("y", function(d) { return obj.h - obj.y(d.values[i].system) - .5 - obj.y(d.values[i].user); })
		    .attr("width", obj.w)
		    .attr("height", function(d) { return obj.y(d.values[i].system); })
		    .attr("class", "system")
		    .transition()
		    .duration(0)
		    .attr("x", function(d, i) { return obj.x(i) - .5; });
		
		rect_user.transition()
		    .duration(0)
		    .attr("color", "red")
		    .attr("x", function(d, i) { return obj.x(i) - .5; })
		    .attr("y", function(d) { return obj.h - obj.y(d.values[i].user) - .5; })
		    .attr("height", function(d) { return obj.y(d.values[i].user); })
		    .attr("class", "user");
		rect_system.transition()
		    .duration(0)
		    .attr("x", function(d, i) { return obj.x(i) - .5; })
		    .attr("y", function(d) { return obj.h - obj.y(d.values[i].system) - .5 - obj.y(d.values[i].user); })
		    .attr("height", function(d) { return obj.y(d.values[i].system); })
		    .attr("class", "system");
		
		rect_user.exit().transition()
		    .duration(0)
		    .attr("x", function(d, i) { return obj.x(i - 1) - .5; })
		    .remove();
		
		rect_system.exit().transition()
		    .duration(0)
		    .attr("x", function(d, i) { return obj.x(i - 1) - .5; })
		    .remove();
	    }
	}
	obj.update(data);
	return obj;
    }
}($);
