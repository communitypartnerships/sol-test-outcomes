const education = d3.csv("1_dataprep/data/cohort_2019.csv");
var ed_data;
var ed_data_filtered;



education.then(function (d) {
        ed_data = d3.nest().key((d) => d.division_use).key((d) => d.demographic_use)
            .key((d) => d.level_use)
            .entries(d); //Nested by division (column) then by cohort (row) then by race (line)
    console.log(ed_data)
    })
    .then(function () {
        draw_viz(ed_data)
    })

function draw_viz(data) {

    // Input list of districts to look at
    var input_list = ["albemarle_county", "charlottesville_city", "fluvanna_county", "nelson_county", "orange_county", "greene_county"]

    for (i in data) {   // label the ones to be kept
        if (input_list.includes(data[i].key)) {
            data[i].name_filter = "keep";
        } else {
            data[i].name_filter = "remove";
        }
    };


    ed_data_filtered = data.filter(d => d.name_filter == "keep"); // run the actual filtering
  console.log(ed_data_filtered)
    // get a list of the divisions in use
    var key = function (d) {
        return d.key;
    }

    // define the graph boundaries
    var margin = {
            top: 50,
            right: 50,
            bottom: 50,
            left: 50
        },
        width = 1000 - margin.left - margin.right,
        height = 1000 - margin.top - margin.bottom,
        usewidth = width + margin.left + margin.right,
        useheight = height + margin.top + margin.bottom;

    // The graph container is the big open one
    var viz_box = d3.select("#graph-container");

//    viz_box.append("div").classed("titlecol", true).;
    
    // each division gets a column 
    var division_cols = viz_box.selectAll(".column")
        .data(ed_data_filtered, key) // the ,key is there to make sure it goes in the same/nameable order
        .enter()
        .append("div")
        .attr("class", (d,i) => "division" + i + " column")
        //        .classed("col-sm-2", true)
        .attr("id", (d) => "column_" + d.key);

    // Each division column gets a name 
    division_cols.append("div").attr("class", "titledivs").append("h3").text((d) => d.values[[0]].values[[0]].values[[0]].division_name);
    
     // Side Labels
// var first_column_boxes =  d3.selectAll(".division0").selectAll(".cohort_box").append("h3").text((d) => d.values[[0]].values[[0]].demographic);

    var cohort_boxes = division_cols.selectAll(".cohort_box")
        .data(d => d.values)
        .enter()
        .append("div")
        .attr("class", (d, i) => d.values[[0]].values[[0]].division_use + " cohort_box" + " row" +i )
        .attr("id", (d) => "cohort" + d.key + d.values[[0]].values[[0]].division_use);

     cohort_boxes.append("h3").text((d) => d.values[[0]].values[[0]].demographic);
    
    var cohort_svg = cohort_boxes
        .append("svg")
        .attr("viewBox", "0 0 " + usewidth + " " + useheight)
        .attr("class", "svg-content")
        .attr("id", (d) => d.key)
        .attr("aria-labelledby", "title");


    // Create Color Scales
    
    // I want to make 13 boxes. 
    number_scale = [];
    var N = 18

    for (var i = 0; i <= N; i++) {
        number_scale.push(i);
    }

    // Turn each box into a height
    var color_scale = d3.scaleLinear().domain([0, N]).range([height, 0])
   
    // sub graph container inside the svg to center it
    var graph_containers = cohort_svg.append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

    var positive_color = d3.scaleLinear().domain([N, N/2, 0]).range([ "#191970", "#106DD1", "rgb(220,243,255)"])

    
  // make the gradient container
    var gradient_containers = graph_containers
         .selectAll(".gradient_containers")
          .data((d) => d.values)
          .enter()
         .append("g")
        .classed("gradient_containers", true)
        .attr("id", function (d) {
            return "gradient_group" + d.values[[0]].demographic_use + d.key + d.values[[0]].division_use
        })
  //   It will be clipped by this path:
        .attr("clip-path", function (d) {
            return "url(#area" +  d.values[[0]].demographic_use + d.key + d.values[[0]].division_use + ")"
        });

    // Draw in the gradient bars themselves
    var positive = gradient_containers
        .selectAll(".positive_rects")
        .data(number_scale).enter().append("rect")
        .attr("x", 0)
        .attr("y", (d) => color_scale(d))
        .attr("width", width)
        .attr("height", height / N)
        .style("fill", (d) => positive_color(d))
        .classed("positive_rects backrect", true);

    var xScale = d3.scaleLinear().domain([1, 10]).range([0, width]);
    var yScale = d3.scaleLinear().domain([10, 100]).range([height, 0])

    // Create the path clippings to divide the positive from the negative gradient
    var gradient_area = d3.area()
        .defined(d => !isNaN(d.pass_rate))
        .x(function (d) {
           return xScale(+d.grade);
        })
        .y0(function (d) {
          
//            return yScale(+d.values[[1]].pass_rate);
            return yScale(+d.pass_rate);
            
        })
        .y1(function (d) {
           
            return yScale(+d.average);
                        

        }).curve(d3.curveMonotoneX);


    gradient_containers

        .append("clipPath")
        .attr("id", function (d) {
            return "area" + d.values[[0]].demographic_use + d.key + d.values[[0]].division_use
        })
        .append("path")
          .attr("class", function(d){ return "areas " + d.key})
          .attr("d", (d) => gradient_area(d.values));

//    var line_color = d3.scaleOrdinal().domain(["White", "All Students", "Black"]).range(["#00008b", "black", "rgb(194, 50, 10)"])

    // Add in lines 

    // Create the line generation function
    var lines = d3.line()
        .defined(d => !isNaN(d.pass_rate))
        .x(function (d) {
            return xScale(+d.grade);
        })
        .y(function (d) {
            return yScale(+d.pass_rate);
        })
        .curve(d3.curveMonotoneX);

    graph_containers
        .selectAll(".linecontainer")
        .data((d) => d.values)
        .enter()
        .append("g")
        .attr("class", function (d) {
            return "linecontainer linescontainer" + d.key
        })
        .append("path")
        .attr("d", (d) => lines(d.values))
        .attr("class", function (d) {
            return "lines line" + d.key
        })
        .attr("stroke", "black" //function (d) {
           // return race_color(d.key)
       // }
             );


    // Add in dots

    var dotcontainers = graph_containers
        .selectAll(".dotcontainer")
        .data((d) => d.values)
        .enter()
        .append("g")
        .attr("class", function (d) {
            return "dotcontainer dots" + d.key
        });

    var doty = function (d) {
        return yScale(+d.pass_rate);
    }

    dotcontainers.selectAll(".dots")
        .data((d) => d.values.filter(q => !isNaN(q.pass_rate)))
        .enter()
        .append("circle")
        .attr("cx", function (d) {
            return xScale(+d.grade);
        })
        .attr("cy", doty)
        .attr("r", 12)
        .style("fill", "black"
//               function (d) {
//            return race_color(d.race)
//        }
              ).attr("class", "dots")


    var xdata = ["3rd", "4th", "5th", "6th", "7th", "8th"]

    // Grades Scale
    graph_containers.append("g")
        .attr("class", "axis")
        .call(d3.axisBottom().scale(xScale).tickValues([3, 4, 5, 6, 7, 8]).tickFormat(function (d, i) {
            return xdata[i];
        }).tickSize(25))
        .attr("transform", "translate(" + 0 + "," + (height - 35) + ")");
    

// Starting value labels
    var start_labely = function (d) {
        return yScale(d.values[[0]].pass_rate)
    };
    
     var start_label_x = function (d) {
        return xScale(d.values[[0]].grade - .3)
    } 
    
    var start_label_text = function (d) {
        return Math.round(d.values[[0]].pass_rate) + "%"
    }
    
    graph_containers.selectAll(".startlabels")
        .data((d) => d.values.filter((q) => !isNaN(q.values[[0]].pass_rate)))
        .enter()
        .append("text")
        .attr("class", "startlabels")
        .text(start_label_text)
        .attr("x", start_label_x)
        .attr("y", start_labely)


    // hover dot labels
    dotcontainers.selectAll(".hovertext")
        .data((d) => d.values.filter(q => !isNaN(q.pass_rate)))
        .enter()
        .append("text")
        .attr("class", "hovertext")
        .text((d) => Math.round(d.pass_rate) + "%")
        .attr("x", function (d) {
            return xScale(+d.grade);
        })
        .attr("y", doty);
    
// Level Labels
    var level_label_y = function (d) {
        return yScale(d.values[[d.values.length -1]].pass_rate )
    };
    
    var level_label_text = function (d) {
        return d.values[[0]].level 
    } 
    
    
    var level_label_x = function (d) {
        return xScale(d.values[[d.values.length -1]].grade - -1*.2)
    } 
    

  graph_containers.selectAll(".level_labels")
        .data((d) => d.values.filter((q) => !isNaN(q.values[[q.values.length -1]].pass_rate)))
        .enter()
        .append("text")
        .attr("class", "level_labels")
        .text(level_label_text)
        .attr("x", level_label_x)
        .attr("y", level_label_y);

    
    
    ///////////////////////////////////////////

    // create checkbox interfaces

    console.log(d3.nest().key((d) => d.values[[0]].values[[0]].values[[0]].region).entries(data))

    var region_nested = d3.nest().key((d) => d.values[[0]].values[[0]].values[[0]].region).entries(data)
    .sort((a,b) => d3.ascending(a.key, b.key))
;


    var checkboxcontainer = d3.select("#checkboxes");

    var checkboxgroups = checkboxcontainer
        .selectAll(".checkboxgroup")
        .data(region_nested)
        .enter()
        .append("div")
        .classed("checkboxgroup", true)

    checkboxgroups
        .append("div")
        .classed("grouptitlediv", true)
        .append("h3")
        .text(d => d.key)
        .classed("checkboxgrouptitle", true)


    var checkboxdivs = checkboxgroups.selectAll("checkbox")
        .data((d) => d.values)
        .enter()
        .append("div")
        .classed("checkdiv", true)

    // add the inputs
    checkboxdivs
        .append("input")
        .attr("type", "checkbox")
        .attr("class", function (d) {
            return d.key + "_checkbox checkbox"
        })
        .attr("value", function (d) {
            return d.key
        })
        .attr("name", function (d) {
            return d.key + "_checkbox"
        })
        .property("checked", function (d) {
            if (d.name_filter == "keep") {
                return true
            } else { // auto check the ones displayed
                return false
            }
        });


    ///////////////////////////////////////////
    // add the labels
    checkboxdivs.append("label")
        .attr("for", function (d) {
            return d.key + "_checkbox"
        })
        .text(function (d) {
            return " " + d.values[0].values[0].values[0].division_name
        }).attr("class", "checkboxlabels")

    // only select 6
    $('input[type=checkbox]').on('change', function (e) {
        if ($('input[type=checkbox]:checked').length > 10) {
            $(this).prop('checked', false);
            alert("Please Only Choose A Maximum of 10");
        }
    });

    
    
    
    ////////////////////////////////////////


    // Swap in and out columns
    //On click, update with new data			
    d3.selectAll("#submitchanges")
        .on("click", function () {

            var checkedboxes = $('input[type=checkbox]:checked')

            var checkboxlist = []
            for (var i = 0; i < checkedboxes.length; i++) {
                checkboxlist[i] = $(checkedboxes[[i]]).val()
            };
            console.log(checkboxlist);

            // update the dataset we are using          
            for (i in data) {
                if (checkboxlist.includes(data[i].key)) {
                    data[i].name_filter = "keep";
                } else {
                    data[i].name_filter = "remove";
                }
            };
            
        
            // refilter the data 
            ed_data_filtered = data.filter(d => d.name_filter == "keep");


            var division_cols = viz_box.selectAll(".column")
                .data(ed_data_filtered, key);
            
        //remove the ones that are exiting
            division_cols.exit()
                .transition()
                .duration(1000)
                .attr("width", 0)
                .remove();

      //append the ones that are entering
            var entering_cols = division_cols.enter()
                .append("div")
                .attr("class", (d,i) => "division" + i + " column")
                .attr("id", (d) => "column_" + d.key);
      
     // each entering column gets a name
            entering_cols.append("div").attr("class", "titledivs").append("h3").text((d) => d.values[[0]].values[[0]].values[[0]].division_name);

        
            var cohort_boxes = entering_cols.selectAll(".cohort_box")
                .data(d => d.values)
                .enter()
                .append("div")
                    .attr("class", (d, i) => d.values[[0]].values[[0]].division_use + " cohort_box" + " row" + i)
                    .attr("id", (d) => "cohort" + d.key + d.values[[0]].values[[0]].division_use);
        
             cohort_boxes.append("h3").text((d) => d.values[[0]].values[[0]].demographic);

            var cohort_svg = cohort_boxes
                .append("svg")
                .attr("viewBox", "0 0 " + usewidth + " " + useheight)
                .attr("class", "svg-content")
                .attr("id", (d) => d.key)
                .attr("aria-labelledby", "title");

            var graph_containers = cohort_svg.append("g")
                .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
      
            
                var gradient_containers = graph_containers
                    .selectAll(".gradient_containers")
                    .data((d) => d.values)
                    .enter()
                    .append("g")
                    .classed("gradient_containers", true)
                    .attr("id", function (d) {
                        return "gradient_group" + d.values[[0]].demographic_use + d.key + d.values[[0]].division_use
                    })
                    //   It will be clipped by this path:
                    .attr("clip-path", function (d) {
                        return "url(#area" + d.values[[0]].demographic_use + d.key + d.values[[0]].division_use + ")"
                    });


       
    // Draw in the gradient bars themselves
    var positive = gradient_containers
        .selectAll(".positive_rects")
        .data(number_scale).enter().append("rect")
        .attr("x", 0)
        .attr("y", (d) => color_scale(d))
        .attr("width", width)
        .attr("height", height / N)
        .style("fill", (d) => positive_color(d))
        .classed("positive_rects backrect", true);


    gradient_containers

        .append("clipPath")
        .attr("id", function (d) {
            return "area" + d.values[[0]].demographic_use + d.key + d.values[[0]].division_use
        })
        .append("path")
          .attr("class", function(d){ return "areas " + d.key})
          .attr("d", (d) => gradient_area(d.values));


            // add in new lines

            graph_containers
                .selectAll(".linecontainer")
                .data((d) => d.values)
                .enter()
                .append("g")
                .attr("class", function (d) {
                    return "linecontainer linescontainer" + d.key
                })
                .append("path")
                .attr("d", (d) => lines(d.values))
                .attr("class", function (d) {
                    return "lines line" + d.key
                })
                .attr("stroke", "black");


            // Add in dots

            var dotcontainers = graph_containers
                .selectAll(".dotcontainer")
                .data((d) => d.values)
                .enter()
                .append("g")
                .attr("class", function (d) {
                    return "dotcontainer dots" + d.key
                });

            dotcontainers.selectAll(".dots")
                .data((d) => d.values.filter(q => !isNaN(q.pass_rate)))
                .enter()
                .append("circle")
                .attr("cx", function (d) {
                    return xScale(+d.grade);
                })
                .attr("cy", doty)
                .attr("r", 12)
                .style("fill", "black").attr("class", "dots")


            //Grades Scale
            graph_containers.append("g")
                .attr("class", "axis")
                .call(d3.axisBottom().scale(xScale).tickValues([3, 4, 5, 6, 7, 8]).tickFormat(function (d, i) {
                    return xdata[i];
                }).tickSize(25))
                .attr("transform", "translate(" + 0 + "," + (height - 35) + ")");
            //    


            // Add Gridlines


            // Add Line Labels to the New ones 
    graph_containers.selectAll(".startlabels")
        .data((d) => d.values.filter((q) => !isNaN(q.values[[0]].pass_rate)))
        .enter()
        .append("text")
        .attr("class", "startlabels")
        .text(start_label_text)
        .attr("x", start_label_x)
        .attr("y", start_labely)


    // hover dot labels
    dotcontainers.selectAll(".hovertext")
        .data((d) => d.values.filter(q => !isNaN(q.pass_rate)))
        .enter()
        .append("text")
        .attr("class", "hovertext")
        .text((d) => Math.round(d.pass_rate) + "%")
        .attr("x", function (d) {
            return xScale(+d.grade);
        })
        .attr("y", doty);
    
// Level Labels
    var level_label_y = function (d) {
        return yScale(d.values[[d.values.length -1]].pass_rate )
    };
    
    var level_label_text = function (d) {
        return d.values[[0]].level 
    } 
    
    
    var level_label_x = function (d) {
        return xScale(d.values[[d.values.length -1]].grade - -1*.2)
    } 
    

  graph_containers.selectAll(".level_labels")
        .data((d) => d.values.filter((q) => !isNaN(q.values[[q.values.length -1]].pass_rate)))
        .enter()
        .append("text")
        .attr("class", "level_labels")
        .text(level_label_text)
        .attr("x", level_label_x)
        .attr("y", level_label_y);
        


        });


    



};







var number_scale;

var yScale;
