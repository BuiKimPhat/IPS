const fetch_metrics = () => {
    const agent_name = JSON.parse(document.getElementById('room-name').textContent);
    const ws = new WebSocket('ws://'+window.location.host+'/ws/ips/'+agent_name+"/");
    ws.onopen = () =>{
      console.log('Connected to Websocket server.')
    };
  
    ws.onmessage = e => {
      const data = JSON.parse(e.data);
      console.log("Message: ", data);
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
  }
  
  connect();