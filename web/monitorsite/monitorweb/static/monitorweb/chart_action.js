const addData = (chart, label, data) => {
    chart.data.labels.push(label);
    chart.data.datasets.forEach((dataset) => {
        dataset.data.push(data);
    });
}

const removeData = chart => {
    chart.data.labels.pop();
    chart.data.datasets.forEach((dataset) => {
        dataset.data.pop();
    });
}

const updateData = (chart, label, data) => {
    const max_data_point = 60;
    if (chart.data.datasets[0].length >= max_data_point) removeData(chart);
    addData(chart, label, data);
    chart.update();
}