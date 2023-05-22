const addData = (chart, label, data) => {
  chart.data.labels.push(label);
  chart.data.datasets.forEach((dataset) => {
      dataset.data.push(data);
  });
}

const removeData = chart => {
  chart.data.labels.shift();
  chart.data.datasets.forEach((dataset) => {
      dataset.data.shift();
  });
}

const updateChart = (chart, label, data) => {
  const max_data_point = 20;
  if (chart.data.datasets[0].data.length >= max_data_point) removeData(chart);
  addData(chart, label, data);
  chart.update();
}

const fetch_metrics = (charts) => {
    const agent_name = JSON.parse(document.getElementById('agent-name').textContent);
    const ws = new WebSocket('ws://'+window.location.host+'/ws/ips/'+agent_name+"/");
    ws.onopen = () =>{
      console.log('Connected to Websocket server.')
    };
  
    ws.onmessage = e => {
      const data = JSON.parse(e.data);
      if (data.type == "metrics_update") {
        updateChart(charts.cpu, new Date(data.timestamp*1000).toLocaleTimeString(), data.cpu_percent);
        updateChart(charts.memory, new Date(data.timestamp*1000).toLocaleTimeString(), data.mem_percent);
        updateChart(charts.diskread, new Date(data.timestamp*1000).toLocaleTimeString(), data.disk_read);
        updateChart(charts.diskwrite, new Date(data.timestamp*1000).toLocaleTimeString(), data.disk_write);
        updateChart(charts.netin, new Date(data.timestamp*1000).toLocaleTimeString(), data.net_in);
        updateChart(charts.netout, new Date(data.timestamp*1000).toLocaleTimeString(), data.net_out);  
      }
    };
  
    ws.onclose = e => {
      console.log('Socket is closed. Reconnect will be attempted in 5 second.', e.reason);
      setTimeout(() => {
        fetch_metrics();
      }, 5000);
    };
  
    ws.onerror = err => {
      console.error('Socket encountered error: ', err.message, '\nClosing socket.');
      ws.close();
    };
  };
  
fetch_metrics(charts);