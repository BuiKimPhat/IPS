// Desktop notification

// Socket
const append_dropdown_child = (message, parent) => {
    let li = document.createElement('li');
}
const fetch_notification = () => {
    const ws = new WebSocket('ws://' + window.location.host + '/ws/ips/notification/');
    ws.onopen = () => {
        console.log('Connected to notification group.')
    };

    ws.onmessage = e => {
        const data = JSON.parse(e.data);
        console.log(data)
        if (data.type == "alert_attack") {
        }
    };

    ws.onclose = e => {
        console.log('Socket was closed. Reconnection will be attempted in 3 seconds.', e.reason);
        setTimeout(() => {
            fetch_notification();
        }, 3000);
    };

    ws.onerror = err => {
        console.error('Socket encountered error: ', err.message, '\nClosing socket.');
        ws.close();
    };
};

fetch_notification();