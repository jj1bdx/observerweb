/*
 Written by Bill Wang <freecnpro@gmail.com>
*/

function loadCharts(){
    const schedulerUtilizationCtx = document.getElementById('scheduler-utilization');
    const memoryUsageCtx = document.getElementById('memory-usage');
    const ioUsageCtx = document.getElementById('io-usage');

    const schedulerChart = new Chart(schedulerUtilizationCtx, {
        type: 'line',
        data: {
            labels: ['Red', 'Blue', 'Yellow', 'Green', 'Purple', 'Orange'], // Labels for the chart
            datasets: [{
                label: 'Votes',
                data: [12, 19, 3, 5, 2, 3]
            }] 
        },
        options: {
            plugins: {
            title: {
                display: true,
                text: 'Scheduler Utilization [%]'
            }
            },
            scales: {
                y: {
                    beginAtZero: true // Start the y-axis at 0
                }
            }
        }
    });
}

function loadSchedulerInfo() {
    var schedulerObj;
    var xmlhttp = new XMLHttpRequest();
    setInterval(function() {
      sendAsyncRequest(xmlhttp, "action=get_perf&type=scheduler", function() {
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
          var newData = eval("(" + xmlhttp.responseText + ")");
          console.log(newData, newData.scheduler.length);
          //  for (var i = 0; i < schedulerObj.scheduler.length; i++) {
          //    var series = schedulerChart.series[i];
          //    var x = (new Date()).getTime(),
          //        activetime = newData.scheduler[i].activetime - schedulerObj.scheduler[i].activetime,
          //        totaltime = newData.scheduler[i].totaltime - schedulerObj.scheduler[i].totaltime,
          //        y = Math.floor((100 * activetime) / totaltime);
          //    series.addPoint([x, y], true, true);
          //    }
          // schedulerObj = newData;
           }
       });
     }, 1000);
}

function loadSysInfo(){
    loadSysInfos();
    setInterval(function(){
        loadSysInfos();
    }, 10*1000);
}

function loadSysInfos() {
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=get_sys", function() {
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
            var jsonData = eval("(" + xmlhttp.responseText + ")");
            var datas = [jsonData.system, jsonData.memory, jsonData.cpu, jsonData.statistics];
            var ids = ["#system-architecture", "#memory-info", "#cpu-threads", "#statistics"];
            for (var i = 0; i < datas.length; i++) {
                displayInfo(ids[i], datas[i]);
            }
        }
    });
}

function loadMAlocInfo() {
    loadMAlocInfos();
    setInterval(function(){
        loadMAlocInfos();
    }, 10*1000);

    const carriersSizeCtx = document.getElementById('carriers-size');
    const carriersUtilizationCtx = document.getElementById('carriers-utilization');
}

function loadMAlocInfos() {
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=get_malloc", function() {
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
            var jsonData = eval("(" + xmlhttp.responseText + ")");

            displayInfo("#alloctor-table", jsonData.allocator);
        }
    });
}

function displayInfo(id, data) {
    var txt = "";
    if (id === "#alloctor-table") {
        for (var i = 0; i < data.length; i++) {
            txt = txt + "<tr><td>" + data[i].name + "</td><td class=\"text-right\">" + data[i].bs + "</td><td class=\"text-right\">" + data[i].cs + "</td></tr>";
        }
    } else {
        for (var i = 0; i < data.length; i++) {
            txt = txt + "<tr><td>" + data[i].name + "</td><td>" + data[i].value + "</td></tr>";
        }
    }
    document.querySelector(id).innerHTML = txt;
}

function sendAsyncRequest(xmlhttp, qs, fun) {
    xmlhttp.onreadystatechange = fun;
    xmlhttp.open("POST", "info", true);
    xmlhttp.send(qs);
}

function sendSyncRequest(qs) {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST", "info", false);
    xmlhttp.send(qs);
    return xmlhttp.responseText;
}

function getTitle(title, max){
    kb = Math.floor(max / 1024);
    mb = Math.floor(kb / 1024);
    gb = Math.floor(mb / 1024);
    if(gb > 10){
        return title+"(GB)";
    }else if(mb > 10){
        return title+"(MB)";
    }else if(kb > 0){
        return title+"(KB)";
    }else{
        return title+"(B)";
    }
}

function getBetterValue(value, max){
    kb = Math.floor(max / 1024);
    mb = Math.floor(kb / 1024);
    gb = Math.floor(mb / 1024);
    if(gb > 10){
        return Math.floor(value / (1024*1024*1024));
    }else if(mb > 10){
        return Math.floor(value / (1024*1024));
    }else if(kb > 0){
        return Math.floor(value / 1024);
    }else{
        return value;
    }
}

function connectNode(){
    var connectModalEl = document.querySelector('#connect_node_modal');
    var connectModal = bootstrap.Modal.getInstance(connectModalEl);
    connectModal.toggle();
    var nodename_selector = document.querySelector('#nodename');
    var nodename = nodename_selector ? nodename_selector.value : "";
    var cookie_selector = document.querySelector('#cookie');
    var cookie = cookie_selector ? cookie_selector.value : "";
    var qs = "action=connect_node&node=" + nodename + "&cookie=" + cookie;
    document.getElementById("connect_node_form").reset();
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, qs, function(){
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
            if(xmlhttp.responseText === "Connect failed"){
                alert("Connect failed!");
            }else{
                location.reload();
            }
        }
    });
}

function getNodes(){
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=get_nodes", function(){
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
            var jsonData = eval("(" + xmlhttp.responseText + ")");
            var nodes = jsonData.nodes;
            var txt = "";
            for(var i = 0; i < nodes.length; i++){
                    txt = txt + "<li><a class=\"dropdown-item\" href=\"#\">" + nodes[i] + "</a></li>";
            }
            document.querySelector('#nodes').innerHTML = txt;
        }
    });
}

function changeNode(node){
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=change_node&node=" + node, function(){
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
            console.log("Response: ", xmlhttp.responseText);
            if(xmlhttp.responseText === "true"){
                location.reload();
            }
        }
    });
}

function loadProInfo(){
    loadProInfos();
    setInterval(function(){
        loadProInfos();
    }, 10*1000);
}

function loadProInfos() {
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=get_pro&type=all", function() {
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
            var jsonData = eval("(" + xmlhttp.responseText + ")");
            var txt = "";
            for (var i = jsonData.length -1; i >= 0 ; i--) {
                txt = txt + "<tr><td>" + jsonData[i].pid + "</td><td>" + jsonData[i].name + "</td><td class=\"text-right\">" + jsonData[i].reds + "</td><td class=\"text-right\">"+ jsonData[i].mem + "</td><td class=\"text-right\">"+ jsonData[i].msg +"</td><td>"+ jsonData[i].fun +"</td></tr>";
            }
            document.querySelector('#process-table').innerHTML = txt;
        }
    });
}

function loadAppVsnInfo() {
    var xmlhttp = new XMLHttpRequest();
    sendAsyncRequest(xmlhttp, "action=get_app_vsn", function() {
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
            var jsonData = eval("(" + xmlhttp.responseText + ")");
            document.querySelector('#vsn').textContent = jsonData.app_vsn;
        }
    });
}

function delegate(el, evt, sel, handler) {
    el.addEventListener(evt, function(event) {
        var t = event.target;
        while (t && t !== this) {
            if (t.matches(sel)) {
                handler.call(t, event);
            }
            t = t.parentNode;
        }
    });
}

function setChangeNode() {
    var ulnodes = document.querySelector('ul#nodes.dropdown-menu');
    // console.log("ChangeNode ulnodes: ", ulnodes);
    delegate(ulnodes, "click", "li a", function(){
        var value = this.innerHTML;
        // console.log("ChangeNode value: ", value);
        if (value !== "Connect Node") {
           changeNode(value);
        }
    });
}
