// Desktop notification

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
                            ("Denied. " ? data.is_denied : "Allowed. ") + data.message;
    div.appendChild(header);
    div.appendChild(content);
    a.appendChild(div)
    li.appendChild(a);
    return li
}

const update_dropdown_list = (data, max_noti) => {
    const noti_dropdown = document.getElementById("alert-notification-dropdown");
    data.alerts.map(item => {
        noti_dropdown.appendChild(dropdown_child(item))
    });
    // Concat if there are existing alerts
    let tmp = data.alerts.concat(Array.from(document.getElementsByTagName('dropdown-item')));
    // Delete existing alerts
    for (let i=1; i<=max_noti; i++){
        noti_dropdown.removeChild(noti_dropdown.chilren[i])
    }
    // Append latest alerts
    for (let i=0; i<max_noti; i++){
        noti_dropdown.appendChild(tmp[i])
    }
    if (noti_dropdown.childElementCount > 2) document.getElementById('no-alert-noti').style.display = 'none';
    else document.getElementById('no-alert-noti').style.display = 'block';
}

const update_badge = (max_noti) => {
    const noti_dropdown = document.getElementById("alert-notification-dropdown");
    const badge = document.getElementById("notification-bell");
    if (noti_dropdown.childElementCount <= 2) badge.style.display = 'none';
    else {
        let tmp_num = noti_dropdown.childElementCount - 2;
        badge.textContent = String(tmp_num) ? tmp_num <= max_noti : String(max_noti)+"+";
        badge.style.display = 'block';
    }
}

const notify = () => {

}

const fetch_notification = () => {
    const ws = new WebSocket('ws://' + window.location.host + '/ws/ips/notification/');
    ws.onopen = () => {
        console.log('Connected to notification group.')
    };

    ws.onmessage = e => {
        const data = JSON.parse(e.data);
        if (data.type == "alert_attack") {
            update_dropdown_list(data, 10);
            update_badge(10);
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