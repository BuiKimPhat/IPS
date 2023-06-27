// Desktop notification
const notify_request = () => {
    if (!window.Notification) {
        console.log('Browser does not support notifications.');
    } else {
        // check if permission is already granted
        if (Notification.permission !== 'granted') {
            // request permission from user
            Notification.requestPermission().then(function (p) {
                if (p === 'granted') {
                    console.log('Desktop notification permission granted.')
                } else {
                    console.log('User blocked notifications.');
                }
            }).catch(function (err) {
                console.error(err);
            });
        }
    }
}

const notify = (data) => {
    data.alerts.forEach(alert => {
        new Notification(alert.rule_class + " attack at agent " + alert.agent_name, {
            body: "Rule triggered: " + alert.rule_name + ". Action: " +
            (alert.is_denied ? "Denied. " : "Allowed. ") + alert.message
        });
    });
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

const toast_child = data => {
    let toast = document.createElement('div');
    toast.className = 'toast';
    toast.role = 'alert';
    toast.ariaLive = 'assertive';
    toast.ariaAtomic = 'true';
    let header = document.createElement('div');
    header.className = 'toast-header';
    let icon = document.createElement('i');
    icon.className = 'bi bi-bell me-2';
    let title = document.createElement('strong');
    title.className = 'me-auto';
    title.textContent = data.rule_class + " attack at agent " + data.agent_name;
    let closeBtn = document.createElement('button');
    closeBtn.type = 'button';
    closeBtn.className = 'btn-close';
    closeBtn.ariaLabel = 'Close';
    closeBtn.setAttribute("data-bs-dismiss", "toast");
    let body = document.createElement('div');
    body.className = 'toast-body';
    body.textContent = "Rule triggered: " + data.rule_name + ". Action: " +
        (data.is_denied ? "Denied. " : "Allowed. ") + data.message;

    header.appendChild(icon);
    header.appendChild(title);
    header.appendChild(closeBtn);
    toast.appendChild(header);
    toast.appendChild(body);
    toast.addEventListener('hidden.bs.toast', () => {
        toast.remove();
    })
    return toast;
}

const update_dropdown_list = (data, max_noti) => {
    const noti_dropdown = document.getElementById("alert-notification-dropdown");
    const exist_alerts = Array.from(document.getElementsByClassName('dropdown-item')).map(item => item.parentElement.cloneNode(true));

    const nodes = data.alerts.map(item => dropdown_child(item));
    // Concat if there are existing alerts
    let tmp = nodes.concat(exist_alerts);
    // Delete existing alerts, don't display the placeholder
    if (exist_alerts.length > 0) {
        for (let i = 0; i < exist_alerts.length; i++) {
            noti_dropdown.removeChild(noti_dropdown.children[1])
        }
    } else noti_dropdown.removeChild(noti_dropdown.children[1]); // Remove "No alert" placeholder
    // Append latest alerts
    for (let i = 0; i < (max_noti <= tmp.length ? max_noti : tmp.length); i++) {
        noti_dropdown.appendChild(tmp[i])
    }
}

const update_badge = (max_noti) => {
    const new_alerts_num = document.getElementsByClassName('dropdown-item').length;
    const badge = document.getElementById("notification-bell");
    if (new_alerts_num == 0) badge.style.display = 'none';
    else {
        badge.textContent = new_alerts_num < max_noti ? new_alerts_num.toString() : max_noti.toString() + "+";
        badge.style.display = 'block';
    }
}

const update_toasts = (data, max_toast) => {
    const toast_container = document.getElementById("toast-container");
    const new_toasts = data.alerts.map(item => toast_child(item));
    let last_full = 0;
    let full_interval = 1000; // miliseconds

    let timestamp = new Date().getTime();

    if (toast_container.childElementCount >= max_toast) {
        if (timestamp - last_full >= full_interval){ 
            for (let i = 0; i < toast_container.childElementCount; i++) {
                const toastBootstrap = bootstrap.Toast.getOrCreateInstance(toast_container.children[i]);
                toastBootstrap.hide();
            }
            for (let i = 0; i < (max_toast <= new_toasts.length ? max_toast : new_toasts.length); i++) {
                toast_container.appendChild(new_toasts[i]);
                const toastBootstrap = bootstrap.Toast.getOrCreateInstance(new_toasts[i]);
                toastBootstrap.show();
            }
            full_interval = timestamp;
        }
    } else {
        for (let i = 0; i < toast_container.childElementCount; i++) {
            const toastBootstrap = bootstrap.Toast.getOrCreateInstance(toast_container.children[i]);
            toastBootstrap.hide();
        }
        for (let i = 0; i < (max_toast <= new_toasts.length ? max_toast : new_toasts.length); i++) {
            toast_container.appendChild(new_toasts[i]);
            const toastBootstrap = bootstrap.Toast.getOrCreateInstance(new_toasts[i]);
            toastBootstrap.show();
        }    
    }

}

const fetch_notification = () => {
    let last_audio = 0;
    let audio_interval = 5000; // miliseconds

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
            update_toasts(data, 4);
            notify(data);
            let timestamp = new Date().getTime();
            if (timestamp - last_audio >= audio_interval){
                document.getElementById('notify-sound').play();
                last_audio = timestamp;
            }
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

window.onload = e => {
    notify_request();
    fetch_notification();
}