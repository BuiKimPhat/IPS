const randomColor = () => {
  let letters = '0123456789ABCDEF';
  let color = '#';
  for (let i = 0; i < 6; i++) {
    color += letters[Math.floor(Math.random() * 16)];
  }
  return color;
}

const isAgentArraySame = (dataset, data) => {
  // Check if agent array changed
  return (
    dataset.length === data.length &&
    dataset.every((e1) =>
      data.some((e2) =>
        e1.label == e2.agent__name
      )
    )
  );
}

const addData = (chart, label, data) => {
  // Reset chart if agent array changed
  if (!isAgentArraySame(chart.data.datasets, data.alert_num)) {
    chart.data.labels = []
    chart.data.datasets = data.alert_num.map(item => {
      return {
        label: item.agent__name,
        data: [item.count],
        fill: false,
        borderColor: randomColor()
      }
    })
  } else {
    // Add data if agent array did not change
    chart.data.datasets.forEach((dataset) => {
      data.alert_num.forEach((item) => {
        if (dataset.label == item.agent__name) dataset.data.push(item.count);
      });
    });  
  }
  chart.data.labels.push(label);
}

const removeData = chart => {
  chart.data.labels.shift();
  chart.data.datasets.forEach((dataset) => {
    dataset.data.shift();
  });
}

const updateChart = (chart, label, data) => {
  const max_data_point = 48;
  addData(chart, label, data);
  if (chart.data.datasets.length > 0 && chart.data.datasets[0].data.length > max_data_point) removeData(chart);
  chart.update();
}

const updateStats = (piechart,data) => {
  document.getElementById("unprocessedNum").textContent = data.unprocessed.toString();
  if (data.unprocessed == 0) document.getElementById("unprocessedNum").style.color = '#4bc0c0';
  else document.getElementById("unprocessedNum").style.color = '#dc3545';
  document.getElementById("ruleNum").textContent = data.rules_set.toString();
  // console.log(data)
  piechart.data.datasets[0].data = [data.healthy, data.agent_num - data.healthy];
  piechart.update();
}

const fetch_stats = (charts) => {
  const ws = new WebSocket('ws://' + window.location.host + '/ws/ips/statistics/');
  ws.onopen = () => {
    console.log('Connected to statistics group.')
  };

  ws.onmessage = e => {
    const data = JSON.parse(e.data);
    // console.log(data);
    if (data.type == "dashboard_update") {
      updateChart(charts.alert_num, data.timestamp, data);
      updateStats(charts.healthy, data);
    }
  };

  ws.onclose = e => {
    console.log('Socket was closed. Reconnection will be attempted in 3 seconds.', e.reason);
    setTimeout(() => {
      fetch_stats();
    }, 3000);
  };

  ws.onerror = err => {
    console.error('Socket encountered error: ', err.message, '\nClosing socket.');
    ws.close();
  };
};

fetch_stats(charts);
