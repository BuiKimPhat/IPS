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
        if (dataset.label == item.agent__name) dataset.data.push(data);
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
  if (chart.data.datasets[0].data.length >= max_data_point) removeData(chart);
  addData(chart, label, data);
  chart.update();
}

const fetch_stats = (chart) => {
  const ws = new WebSocket('ws://' + window.location.host + '/ws/ips/statistics/');
  ws.onopen = () => {
    console.log('Connected to statistics group.')
  };

  ws.onmessage = e => {
    const data = JSON.parse(e.data);
    console.log(data);
    if (data.type == "dashboard_update") {
      updateChart(chart, data.timestamp, data.alert_num);
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

fetch_stats(alertChart);