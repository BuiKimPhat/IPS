// Desktop notification
const notify = () => {

}
// Socket
const dropdown_child = data => {
    let li = document.createElement('li');
    let a = document.createElement('a');
    a.href = "#";
    a.className = "dropdown-item";
    let div = document.createElement('div');
    let header = document.createElement('b');
    header.textContent = data.rule_class + " attack at agent " + data.agent_name;
    header.className = "m-0";
    let content = document.createElement('p');
    content.className = "m-0";
    content.textContent = "Rule triggered: " + data.rule_name + ". Action: " + 
                            (data.is_denied ? "Denied. " : "Allowed. ") + data.message;
    div.appendChild(header);
    div.appendChild(content);
    a.appendChild(div)
    li.appendChild(a);
    return li
}

const update_dropdown_list = (data, max_noti) => {
    const noti_dropdown = document.getElementById("alert-notification-dropdown");
    const exist_alerts = Array.from(document.getElementsByClassName('dropdown-item')).map(item => item.parentElement.cloneNode(true));
    
    const nodes = data.alerts.map(item => dropdown_child(item));
    // Concat if there are existing alerts
    let tmp = nodes.concat(exist_alerts);
    // Delete existing alerts, don't display the placeholder
    if (exist_alerts.length > 0) {
        for (let i=0; i < exist_alerts.length; i++){
            noti_dropdown.removeChild(noti_dropdown.children[1])
        }    
    } else noti_dropdown.removeChild(noti_dropdown.children[1]); // Remove "No alert" placeholder
    // Append latest alerts
    for (let i=0; i < (max_noti <= tmp.length ? max_noti : tmp.length); i++){
        noti_dropdown.appendChild(tmp[i])
    }
}

const update_badge = (max_noti) => {
    const new_alerts_num = document.getElementsByClassName('dropdown-item').length;
    const badge = document.getElementById("notification-bell");
    if (new_alerts_num == 0) badge.style.display = 'none';
    else {
        badge.textContent = new_alerts_num < max_noti ? new_alerts_num.toString() : max_noti.toString()+"+";
        badge.style.display = 'block';
    }
}

const fetch_notification = () => {
    update_badge(15);
    const ws = new WebSocket('ws://' + window.location.host + '/ws/ips/notification/');
    ws.onopen = () => {
        console.log('Connected to notification group.')
    };

    ws.onmessage = e => {
        const data = JSON.parse(e.data);
        if (data.type == "alert_attack") {
            update_dropdown_list(data, 15);
            update_badge(15);
            document.getElementById('notify-sound').play();
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