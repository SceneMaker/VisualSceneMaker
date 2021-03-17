import {useEffect, useState} from 'react';
import './App.css';


function App() {
  const [text, settext] = useState("");
  const [ws, setws] = useState();
  const [state, setState] = useState();
  useEffect(() => {
    let ws = new WebSocket('ws://' + document.location.host + '/ws');

    settext('Connecting...')
    ws.onopen = function () {
      settext('Connected!');
    };
    ws.onclose = function () {
      settext('Lost connection');
    };
    ws.onmessage = function (msg) {
      console.log(msg.data)
      const parts = msg.data.split('#');
      const command = parts[1];
      if (command === "REQUEST") {
        setState({
          action: command,
          variable: parts[3],
          options: parts[4].split(','),
          timestamp: parts[2]
        })
      }
      settext(msg.data);
    };
    setws(ws);
  }, []);

  function sendmsg(e) {
    e.preventDefault();
    ws.send(e.target.in.value)
  }

  function sendVar(e) {
    e.preventDefault();
    const x = Array.from(e.target.selection.elements)
    const res = x.find((i) => i.checked).value
    if (res) {
      ws.send(`VSMMessage#VAR#${e.target.var.value}#${res}`)
    }
  }

  function sendGo() {
    ws.send("VSMMessage#Go")
  }

  return (
      < div
  className = "App" >
      < header
  className = "App-header" >
      {/* <img src={logo} className="App-logo" alt="logo" /> */}
      < button
  onClick = {sendGo} > Go < /button>
      < form
  onSubmit = {sendmsg} >
      < input
  id = 'in' >< /input>
      < button
  type = 'submit' > send < /button>
  {
    text
  }
<
  /form>
  {
    (state && (state.action === "REQUEST")) &&
    < form
    onSubmit = {sendVar} >
        For
    var <
    input
    value = {state.variable}
    id = "var" > < /input>
        < fieldset
    id = 'selection' >
        {state.options.map((option) => < > < input type = 'radio' value = {option}
    /><label>{option}</
    label > < />)}
    < /fieldset>
    < button > submit < /button>
    < /form>
  }
  <
    /header>
    < /div>
  )
  ;
}

export default App;
