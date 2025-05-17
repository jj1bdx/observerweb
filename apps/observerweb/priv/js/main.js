// Originally written by Bill Wang <freecnpro@gmail.com>
// Revised for chart.js by Kenji Rikitake

function loadCharts() {

  var schedulerobj, ioobj;

  // Scheduler Chart init
  const schedulerChart = new Chart(document.getElementById("scheduler-utilization"), {
    type: "line",
    data: function() {
      const size = 60;
      var labels = [],
        time = new Date().getTime(),
        j;
      for (j = -size; j <= 0; j += 1) {
        labels.push(time + j * 1000);
      }
      var responseText = sendSyncRequest("action=get_perf&type=scheduler");
      schedulerobj = eval("(" + responseText + ")");
      var seriesdata = [];
      for (var i = 0; i < schedulerobj.scheduler.length; i++) {
        seriesdata.push({
          label: String(i + 1),
          data: function() {
            var data = [],
              j;
            for (j = -size; j <= 0; j += 1) {
              data.push(0);
            }
            return data;
          }()
        });
      }
      return {
        labels: labels,
        datasets: seriesdata
      };
    }(),
    options: {
      animation: false,
      locale: "en-GB",
      plugins: {
        title: {
          align: "start",
          font: {
            weight: "bold",
            size: 16
          },
          display: true,
          text: "Scheduler Utilization [%]"
        },
        tooltip: false
      },
      scales: {
        x: {
          type: "time"
        },
        y: {
          beginAtZero: true,
          min: 0,
          max: 100
        }
      },
      datasets: {
        line: {
          pointRadius: 0
        }
      },
      elements: {
        point: {
          radius: 0
        }
      }
    }
  });
  schedulerChart.update("none");

  // Scheduler chart periodic update
  var xmlhttp = new XMLHttpRequest();
  setInterval(function() {
    sendAsyncRequest(xmlhttp, "action=get_perf&type=scheduler", function() {
      if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
        var newData = eval("(" + xmlhttp.responseText + ")");
        var labels = schedulerChart.data.labels;
        var x = new Date().getTime();
        labels.push(x);
        labels.shift();
        for (var i = 0; i < schedulerobj.scheduler.length; i++) {
          var series = schedulerChart.data.datasets[i].data;
          var x = new Date().getTime(),
            activetime = newData.scheduler[i].activetime - schedulerobj.scheduler[i].activetime,
            totaltime = newData.scheduler[i].totaltime - schedulerobj.scheduler[i].totaltime,
            y = Math.floor(100 * activetime / totaltime);
          series.push(y);
          series.shift();
        }
        schedulerobj = newData;
        schedulerChart.update("none");
      }
    });
  }, 1000);

  // Memory char init
  const memoryChart = new Chart(document.getElementById("memory-usage"), {
    type: "line",
    data: function() {
      const size = 60;
      var labels = [],
        time = new Date().getTime(),
        j;
      for (j = -size; j <= 0; j += 1) {
        labels.push(time + j * 1000);
      }
      var seriesdata = [];
      var names = ["Total", "Processes", "Atom", "Binary", "Code", "Ets"];
      for (var i = 0; i < names.length; i++) {
        seriesdata.push({
          label: names[i],
          data: function() {
            var data = [],
              j;
            for (j = -size; j <= 0; j += 1) {
              data.push(0);
            }
            return data;
          }()
        });
      }
      return {
        labels: labels,
        datasets: seriesdata
      };
    }(),
    options: {
      animation: false,
      locale: "en-GB",
      plugins: {
        title: {
          align: "start",
          font: {
            weight: "bold",
            size: 16
          },
          display: true,
          text: "Memory Usage [MB]"
        },
        tooltip: false
      },
      scales: {
        x: {
          type: "time"
        },
        y: {
          beginAtZero: true,
          min: 0
        }
      },
      datasets: {
        line: {
          pointRadius: 0
        }
      },
      elements: {
        point: {
          radius: 0
        }
      }
    }
  });
  memoryChart.update("none");

  // Memory chart periodic update
  var xmlhttp = new XMLHttpRequest();
  setInterval(function() {
    sendAsyncRequest(xmlhttp, "action=get_perf&type=memory", function() {
      if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
        var newData = eval("(" + xmlhttp.responseText + ")");
        var labels = memoryChart.data.labels;
        var x = new Date().getTime();
        labels.push(x);
        labels.shift();
        var values = [newData.total, newData.processes, newData.atom, newData.binary, newData.code, newData.ets];
        var max = values.sort(function(a, b) {
          return b - a;
        })[0];
        for (var i = 0; i < values.length; i++) {
          var series = memoryChart.data.datasets[i].data;
          series.push(values[i] / 1048576);
          series.shift();
        }
        memoryChart.update("none");
      }
    });
  }, 1000);

  // I/O Chart init
  const ioChart = new Chart(document.getElementById("io-usage"), {
    type: "line",
    data: function() {
      const size = 60;
      var labels = [],
        time = new Date().getTime(),
        j;
      for (j = -size; j <= 0; j += 1) {
        labels.push(time + j * 1000);
      }
      var seriesdata = [];
      var responseText = sendSyncRequest("action=get_perf&type=io");
      ioobj = eval("(" + responseText + ")");
      var names = ["Input", "Output"];
      for (var i = 0; i < names.length; i++) {
        seriesdata.push({
          label: names[i],
          data: function() {
            var data = [],
              j;
            for (j = -size; j <= 0; j += 1) {
              data.push(0);
            }
            return data;
          }()
        });
      }
      return {
        labels: labels,
        datasets: seriesdata
      };
    }(),
    options: {
      animation: false,
      locale: "en-GB",
      plugins: {
        title: {
          align: "start",
          font: {
            weight: "bold",
            size: 16
          },
          display: true,
          text: "IO Usage [KB]"
        },
        tooltip: false
      },
      scales: {
        x: {
          type: "time"
        },
        y: {
          beginAtZero: true,
          min: 0
        }
      },
      datasets: {
        line: {
          pointRadius: 0
        }
      },
      elements: {
        point: {
          radius: 0
        }
      }
    }
  });
  ioChart.update("none");

  // I/O chart periodic update
  var xmlhttp = new XMLHttpRequest();
  setInterval(function() {
    sendAsyncRequest(xmlhttp, "action=get_perf&type=io", function() {
      if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
        var newData = eval("(" + xmlhttp.responseText + ")");
        var labels = ioChart.data.labels;
        var x = new Date().getTime();
        labels.push(x);
        labels.shift();
        var input = newData.input - ioobj.input;
        var output = newData.output - ioobj.output;
        var max = input > output ? input : output;
        var inputSeries = ioChart.data.datasets[0].data;
        var outputSeries = ioChart.data.datasets[1].data;
        inputSeries.push(input / 1024);
        inputSeries.shift();
        outputSeries.push(output / 1024);
        outputSeries.shift();
        ioobj = newData;
        ioChart.update("none");
      }
    });
  }, 1000);
}

// System Info table init
function loadSysInfo() {
  loadSysInfos();
  setInterval(function() {
    loadSysInfos();
  }, 10 * 1000);
}

// System Info table periodic update 
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
  setInterval(function() {
    loadMAlocInfos();
  }, 10 * 1000);

  // Carrier Size chart init
  const sizeChart = new Chart(document.getElementById("carriers-size"), {
    type: "line",
    data: function() {
      const size = 60;
      var labels = [],
        time = new Date().getTime(),
        j;
      for (j = -size; j <= 0; j += 1) {
        labels.push(time + j * 1000);
      }
      var seriesdata = [];
      var names = ["Total", "Temp", "Sl", "Std", "Ll", "Eheap", "Ets", "Fix", "Literal", "Binary", "Driver"];
      for (var i = 0; i < names.length; i++) {
        seriesdata.push({
          label: names[i],
          data: function() {
            var data = [],
              j;
            for (j = -size; j <= 0; j += 1) {
              data.push(0);
            }
            return data;
          }()
        });
      }
      return {
        labels: labels,
        datasets: seriesdata
      };
    }(),
    options: {
      animation: false,
      locale: "en-GB",
      plugins: {
        title: {
          align: "start",
          font: {
            weight: "bold",
            size: 16
          },
          display: true,
          text: "Carrier Size [MB]"
        },
        tooltip: false
      },
      scales: {
        x: {
          type: "time"
        },
        y: {
          beginAtZero: true,
          min: 0
        }
      },
      datasets: {
        line: {
          pointRadius: 0
        }
      },
      elements: {
        point: {
          radius: 0
        }
      }
    }
  });
  sizeChart.update("none");

  // Carrier Utilization chart init
  const utiliChart = new Chart(document.getElementById("carriers-utilization"), {
    type: "line",
    data: function() {
      const size = 60;
      var labels = [],
        time = new Date().getTime(),
        j;
      for (j = -size; j <= 0; j += 1) {
        labels.push(time + j * 1000);
      }
      var seriesdata = [];
      var names = ["Total", "Temp", "Sl", "Std", "Ll", "Eheap", "Ets", "Fix", "Literal", "Binary", "Driver"];
      for (var i = 0; i < names.length; i++) {
        seriesdata.push({
          label: names[i],
          data: function() {
            var data = [],
              j;
            for (j = -size; j <= 0; j += 1) {
              data.push(0);
            }
            return data;
          }()
        });
      }
      return {
        labels: labels,
        datasets: seriesdata
      };
    }(),
    options: {
      animation: false,
      locale: "en-GB",
      plugins: {
        title: {
          align: "start",
          font: {
            weight: "bold",
            size: 16
          },
          display: true,
          text: "Carrier Utilization [%]"
        },
        tooltip: false
      },
      scales: {
        x: {
          type: "time"
        },
        y: {
          beginAtZero: true,
          min: 0,
          max: 100
        }
      },
      datasets: {
        line: {
          pointRadius: 0
        }
      },
      elements: {
        point: {
          radius: 0
        }
      }
    }
  });
  utiliChart.update("none");

  // Carrier Size/Utilization chart periodic update
  var xmlhttp = new XMLHttpRequest();
  setInterval(function() {
    sendAsyncRequest(xmlhttp, "action=get_malloc", function() {
      if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
        var newData = eval("(" + xmlhttp.responseText + ")");
        var sizeLabels = sizeChart.data.labels;
        var utiliLabels = utiliChart.data.labels;
        var x = new Date().getTime();
        sizeLabels.push(x);
        sizeLabels.shift();
        utiliLabels.push(x);
        utiliLabels.shift();
        var allocators = newData.allocator;
        for (var i = 0; i < allocators.length; i++) {
          var sizeSeries = sizeChart.data.datasets[i].data;
          var utiliSeries = utiliChart.data.datasets[i].data;
          sizeSeries.push(allocators[i].cs / 1024);
          utiliSeries.push((allocators[i].bs / allocators[i].cs) * 100);
          sizeSeries.shift();
          utiliSeries.shift();
        }
        sizeChart.update("none");
        utiliChart.update("none");
      }
    });
  }, 1000);
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
      txt = txt + "<tr><td>" + data[i].name + '</td><td class="text-right">' + data[i].bs + '</td><td class="text-right">' + data[i].cs + "</td></tr>";
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

function connectNode() {
  var connectModalEl = document.querySelector("#connect_node_modal");
  var connectModal = bootstrap.Modal.getInstance(connectModalEl);
  connectModal.toggle();
  var nodename_selector = document.querySelector("#nodename");
  var nodename = nodename_selector ? nodename_selector.value : "";
  var cookie_selector = document.querySelector("#cookie");
  var cookie = cookie_selector ? cookie_selector.value : "";
  var qs = "action=connect_node&node=" + nodename + "&cookie=" + cookie;
  document.getElementById("connect_node_form").reset();
  var xmlhttp = new XMLHttpRequest();
  sendAsyncRequest(xmlhttp, qs, function() {
    if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
      if (xmlhttp.responseText === "Connect failed") {
        alert("Connect failed!");
      } else {
        location.reload();
        alert("Connect to Node " + nodename);
      }
    }
  });
}

function getNodes() {
  var xmlhttp = new XMLHttpRequest();
  sendAsyncRequest(xmlhttp, "action=get_nodes", function() {
    if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
      var jsonData = eval("(" + xmlhttp.responseText + ")");
      var nodes = jsonData.nodes;
      var txt = "";
      for (var i = 0; i < nodes.length; i++) {
        txt = txt + '<li><a class="dropdown-item" href="#">' + nodes[i] + "</a></li>";
      }
      document.querySelector("#nodes").innerHTML = txt;
    }
  });
}

function changeNode(node) {
  var xmlhttp = new XMLHttpRequest();
  sendAsyncRequest(xmlhttp, "action=change_node&node=" + node, function() {
    if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
      console.log("Response: ", xmlhttp.responseText);
      if (xmlhttp.responseText === "true") {
        location.reload();
      }
    }
  });
}

function loadProInfo() {
  loadProInfos();
  setInterval(function() {
    loadProInfos();
  }, 10 * 1000);
}

function loadProInfos() {
  var xmlhttp = new XMLHttpRequest();
  sendAsyncRequest(xmlhttp, "action=get_pro&type=all", function() {
    if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
      var jsonData = eval("(" + xmlhttp.responseText + ")");
      var txt = "";
      for (var i = jsonData.length - 1; i >= 0; i--) {
        txt = txt + "<tr><td>" + jsonData[i].pid + "</td><td>" + jsonData[i].name + '</td><td class="text-right">' + jsonData[i].reds + '</td><td class="text-right">' + jsonData[i].mem + '</td><td class="text-right">' + jsonData[i].msg + "</td><td>" + jsonData[i].fun + "</td></tr>";
      }
      document.querySelector("#process-table").innerHTML = txt;
    }
  });
}

function loadAppVsnInfo() {
  var xmlhttp = new XMLHttpRequest();
  sendAsyncRequest(xmlhttp, "action=get_app_vsn", function() {
    if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
      var jsonData = eval("(" + xmlhttp.responseText + ")");
      document.querySelector("#vsn").textContent = jsonData.app_vsn;
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
  var ulnodes = document.querySelector("ul#nodes.dropdown-menu");
  delegate(ulnodes, "click", "li a", function() {
    var value = this.innerHTML;
    if (value !== "Connect Node") {
      changeNode(value);
    }
  });
}

// Startup function
// for DOMContentLoaded event 

function startUp() {
  getNodes();
  loadAppVsnInfo();
  loadSysInfo();
  setTimeout(function() {
    loadCharts();
  }, 2000);
  loadMAlocInfo();
  loadProInfo();
  setChangeNode();
}
