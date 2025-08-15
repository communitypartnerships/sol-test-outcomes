const tornado = d3.csv("../assets/data/tornado.csv");
var tornado_data;
var tornado_data_filtered;
var year_var = 2006;
var demographic_var = "Race";

tornado.then(function (d) {
        tornado_data = d;
        // console.log(tornado_data)
    })
    .then(function () {
        draw_tornado(tornado_data, demographic_var, year_var);
    })

function draw_tornado(data, demographic, year) {

    tornado_data_filtered =
        d3.nest().key((d) => d.division_use).entries(
            data.filter((d) => d.test_year == year & d.demographic == demographic)
        );
    console.log(tornado_data_filtered)



    // define the graph boundaries
    var margin = {
            top: 200,
            right: 0,
            bottom: 50,
            left: 0
        },
        width = 5000 - margin.left - margin.right,
        height = 5000 - margin.top - margin.bottom,
        usewidth = width + margin.left + margin.right,
        useheight = height + margin.top + margin.bottom;

    // The graph container is the big open one
    var viz_box = d3.select("#tornado-container");

    var viz_svg = viz_box
        .append("svg")
        .attr("viewBox", "0 0 " + usewidth + " " + useheight)
        .attr("class", "svg-content")
        .attr("id", "viz_svg");


    var lowerX = .4;
    var yScale = d3.scaleLinear().domain([135, 1]).range([height, 0]);
    var xScale = d3.scaleLinear().domain([lowerX, 1]).range([0, width]);


    var N = 6
    var xlines = [];

    for (var i = 0; i <= N; i++) {
        xlines.push(i);
    }

    var xlinescale = d3.scaleLinear().domain([0, N]).range([lowerX, 1])

    var axes_g = viz_svg.append("g").attr("transform", "translate(" + margin.left + "," + (margin.top - 50) + ")")

    var vert_lines = axes_g.selectAll(".vert_lines")
        .data(xlines)
        .enter()
        .append("line")
        .attr("x1", (d) => xScale(xlinescale(+d)))
        .attr("x2", (d) => xScale(xlinescale(+d)))
        .attr("y1", 0)
        .attr("y2", height)
        .attr("class", "vert_lines");

    var vert_line_text =
        axes_g.selectAll(".vert_line_text")
        .data(xlines)
        .enter()
        .append("text")
        .attr("x", (d) => xScale(xlinescale(+d)))
        .attr("y", -20)
        .text((d) => Math.round(xlinescale(+d) * 100) + "%")
        .attr("class", "vert_line_text")


    var color_scale = d3.scaleOrdinal().domain(["black", "white", "disadvantaged", "other", "hispanic"]).range(["green", "purple", "green", "purple", "purple"]);
    var legend_g = viz_svg.append("g").attr("class", "legend").attr("transform", "translate(" + (width / 2 - 150) + "," + 0 + ")")

    var legend_keys = d3.map(tornado_data_filtered[0].values, (d) => d.level).keys();

    legend_g.selectAll(".legend_groups")
        .data(legend_keys)
        .enter()
        .append("circle")
        .attr("cx", function (g, i) {
            return i * 350
        })
        .attr("cy", 0) // 100 is where the first dot appears. 25 is the distance between dots
        .attr("r", 30)
        .style("fill", function (g) {
            return color_scale(g)
        })

    legend_g.selectAll("mylabels")
        .data(legend_keys)
        .enter()
        .append("text")
        .attr("x", function (d, i) {
            return 50 + i * 350
        })
        .attr("y", 0) // 100 is where the first dot appears. 25 is the distance between dots
        //    .style("fill", function(d){ return color_scale(d)})
        .text(function (d) {
            return humanize(d)
        })

        .attr("class", "legend_labels");


    var viz_g = viz_svg
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")")


    // get a list of the divisions in use
    var key = function (d) {
        return d.key;
    }

    var division_boxes = viz_g.selectAll(".division_boxes")
        .data(tornado_data_filtered, key)
        .enter()
        .append("g")
        .attr("class", "division_boxes")
        .attr("id", (d) => d.key);


    var lines_tornado = d3.line()
        //        .defined(d => !isNaN(d.pass_rate))
        .x(function (d) {
            return xScale(+d.rate);
        })
        .y(function (d) {
            return yScale(+d.rank);
        })
        .curve(d3.curveMonotoneX);

    division_boxes.append("text")
        .attr("class", "division_label")
        .attr("x", xScale(.98)
            //          (d) => xScale( Math.max( +d.values[0].rate, +d.values[1].rate   )   )
        )
        .attr("y", (d) => yScale(+d.values[0].rank))
        .text((d) => d.values[0].division_name)


    division_boxes.append("path")
        .attr("d", (d) => lines_tornado(d.values))
        .attr("class", function (d) {
            return "lines_tornado line" + d.key
        });


    division_boxes.selectAll(".division_dots")
        .data((d) => d.values)
        .enter()
        .append("circle")
        .attr("class", "division_dots")
        .attr("r", 15)
        .attr("cx", (d) => xScale(+d.rate))
        .attr("cy", (d) => yScale(+d.rank))
        .attr("fill", (d) => color_scale(d.level))


    division_boxes.selectAll(".division_text")
        .data((d) => d.values)
        .enter()
        .append("text")
        .attr("class", "division_text")
        //    .attr("r", 15)
        .attr("x",
            function (d) {
                if (d.rate == d.max_rate) {
                    return xScale(+d.rate) + 100
                } else {

                    return xScale(+d.rate) - 100

                }
            })
        .attr("y", (d) => yScale(+d.rank))
        .text((d) => Math.round(+d.rate * 100) + "%")


    // Tornado highlighting interactions


    var TagList = d3.map(tornado_data, function (d) {
        return d.division_use;
    }).keys().sort(d3.ascending);

    var addon = ["   "];
    TagList = addon.concat(TagList);

    var tagselector = d3.select(".division_filter")
        .append("select")
        .classed("form-control", true)
        .classed("division_selector", true)
        .on("change", function () {
            d3.selectAll(".division_boxes").classed("highlighted", false);
            d3.selectAll("#" + this.value).classed("highlighted", true);
            console.log(this.value);
        });

    function humanize(str) {
        var i, frags = str.split('_');
        for (i = 0; i < frags.length; i++) {
            frags[i] = frags[i].charAt(0).toUpperCase() + frags[i].slice(1);
        }
        return frags.join(' ');
    }

    tagselector.selectAll("option")
        .data(TagList)
        .enter()
        .append("option")
        .attr("value", (d) => d)
        .text((d) => humanize(d))
        .classed("tagoption", true);


    // Update Year
    d3.selectAll("#plus_year")
        .on("click", function () {

            if (year_var <= 2018) {
                year_var = year_var + 1
            } else {
                year_var = 2019
            }

            console.log(year_var);

            update_year(data, year_var)

        });

    d3.selectAll("#minus_year")
        .on("click", function () {

            if (year_var >= 2007) {
                year_var = year_var - 1
            } else {
                year_var = 2006
            }

            console.log(year_var);

            update_year(data, year_var)
        });




    // Update years thing.

    function update_year(data, year_input) {

        d3.select("#year_display").text("Current Year: " + year_input);
        tornado_data_filtered =
            d3.nest().key((d) => d.division_use).entries(
                data.filter((d) => d.test_year == year_input & d.demographic == demographic_var)
            );

        console.log(tornado_data_filtered)

        var division_boxes = viz_g.selectAll(".division_boxes")
            .data(tornado_data_filtered, key);

        //remove the ones that are exiting
        division_boxes.exit()
            .transition()
            .remove();

        //append the ones that are entering
        var entering_divisions = division_boxes.enter()
            .append("g")
            .attr("class", "division_boxes")
            .attr("id", (d) => d.key);

        // each entering division gets a name
        entering_divisions.append("text")
            .attr("class", "division_label")
            .attr("x", xScale(.98)
                //          (d) => xScale( Math.max( +d.values[0].rate, +d.values[1].rate   )   )
            )
            .attr("y", (d) => yScale(+d.values[0].rank))
            .text((d) => d.values[0].division_name)


        var division_boxes = viz_g.selectAll(".division_boxes")
            .data(tornado_data_filtered, key)


        division_boxes
            .selectAll(".division_label")
            .data(tornado_data_filtered, key)
            .transition()
            .attr("y", (d) => yScale(+d.values[0].rank))


        division_boxes.selectAll(".division_dots")
            .data((d) => d.values)
            .transition()
            .attr("cx", (d) => xScale(+d.rate))
            .attr("cy", (d) => yScale(+d.rank))


        division_boxes.selectAll(".lines_tornado")
            .data(tornado_data_filtered, key)
            .transition()
            .attr("d", (d) => lines_tornado(d.values))

        division_boxes.selectAll(".division_text")
            .data((d) => d.values)
            .transition()
            .attr("x",
                function (d) {
                    if (d.rate == d.max_rate) {
                        return xScale(+d.rate) + 100
                    } else {

                        return xScale(+d.rate) - 100

                    }
                })
            .attr("y", (d) => yScale(+d.rank))
            .text((d) => Math.round(+d.rate * 100) + "%")


    }

    // Update Demographics

    var DemList = d3.map(tornado_data, function (d) {
        return d.demographic;
    }).keys();

    //   var addon = ["   "];
    //  DemList = addon.concat(TagList);

    var demographic_selector = d3.select(".demographic_filter")
        .append("select")
        .classed("form-control", true)
        .classed("division_selector", true)
        .on("change", function () {

            demographic_var = this.value;
            update_demographic(data, demographic_var);

            console.log(demographic_var);
        });

    demographic_selector.selectAll("option")
        .data(DemList)
        .enter()
        .append("option")
        .attr("value", (d) => d)
        .text((d) => humanize(d))
        .classed("tagoption", true);



    function update_demographic(data, demographic_input) {

        //        d3.select("#year_display").text("Current Year: " + year_input);
        tornado_data_filtered =
            d3.nest().key((d) => d.division_use).entries(
                data.filter((d) => d.test_year == year_var & d.demographic == demographic_input)
            );

        console.log(tornado_data_filtered)

        var division_boxes = viz_g.selectAll(".division_boxes")
            .data(tornado_data_filtered, key);

        //remove the ones that are exiting
        division_boxes.exit()
            .transition()
            .remove();

        //append the ones that are entering
        var entering_divisions = division_boxes.enter()
            .append("g")
            .attr("class", "division_boxes")
            .attr("id", (d) => d.key);

        // each entering division gets a name
        entering_divisions.append("text")
            .attr("class", "division_label")
            .attr("x", xScale(.98)
                //          (d) => xScale( Math.max( +d.values[0].rate, +d.values[1].rate   )   )
            )
            .attr("y", (d) => yScale(+d.values[0].rank))
            .text((d) => d.values[0].division_name)


        var division_boxes = viz_g.selectAll(".division_boxes")
            .data(tornado_data_filtered, key)


        division_boxes
            .selectAll(".division_label")
            .data(tornado_data_filtered, key)
            .transition()
            .attr("y", (d) => yScale(+d.values[0].rank))


        division_boxes.selectAll(".division_dots")
            .data((d) => d.values)
            .transition()
            .attr("cx", (d) => xScale(+d.rate))
            .attr("cy", (d) => yScale(+d.rank))


        division_boxes.selectAll(".lines_tornado")
            .data(tornado_data_filtered, key)
            .transition()
            .attr("d", (d) => lines_tornado(d.values))

        division_boxes.selectAll(".division_text")
            .data((d) => d.values)
            .transition()
            .attr("x",
                function (d) {
                    if (d.rate == d.max_rate) {
                        return xScale(+d.rate) + 100
                    } else {

                        return xScale(+d.rate) - 100

                    }
                })
            .attr("y", (d) => yScale(+d.rank))
            .text((d) => Math.round(+d.rate * 100) + "%")


        var legend_keys = d3.map(tornado_data_filtered[0].values, (d) => d.level).keys()//.reverse();

        legend_g.selectAll("circle").remove()

        legend_g.selectAll(".legend_groups")
            .data(legend_keys)
            .enter()
            .append("circle")
            .attr("cx", function (g, i) {
                return -100 + i * 600
            })
            .attr("cy", 0) // 100 is where the first dot appears. 25 is the distance between dots
            .attr("r", 30)
            .style("fill", function (g) {
                return color_scale(g)
            })

        legend_g.selectAll(".legend_labels").remove()

        legend_g.selectAll("legend_labels")
            .data(legend_keys)
            .enter()
            .append("text")
            .attr("x", function (d, i) {
                return  -50 + i * 600
            })
            .attr("y", 0) // 100 is where the first dot appears. 25 is the distance between dots
            //    .style("fill", function(d){ return color_scale(d)})
            .text(function (d) {
                return humanize(d)
            })

            .attr("class", "legend_labels");


    }





};
