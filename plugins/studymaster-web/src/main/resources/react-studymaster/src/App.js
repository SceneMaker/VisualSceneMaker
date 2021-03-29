import {useEffect, useState} from 'react';
import './App.css';


function App() {
  const [text, settext] = useState("");
  const [ws, setws] = useState();
  const [state, setState] = useState();
  const inputValue = new Map();
  useEffect(() => {
    let ws = new WebSocket('ws://' + document.location.host + '/ws');

    settext('Connecting...');
    ws.onopen = function () {
      settext('Connected!');
    };
    ws.onclose = function () {
      settext('Lost connection');
    };
    ws.onmessage = function (msg) {
      console.log(msg.data);
      const parts = msg.data.split('#');
      const command = parts[1];
      if (command === "REQUEST") {
        setState({
          action: command,
          variable: parts[3].split(';'),
          options: parts[4].split(';'),
          type: parts[5].split(';'),
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

  function sendSubmit(e){
      ws.send(`VSMMessage#VAR#request_result#SUBMIT`);
      sendVar(e);
  }

  function sendCancel(e){
      ws.send(`VSMMessage#VAR#request_result#CANCEL`);
      sendVar(e);
  }

  // If submit or cancel button is being pushed:
  // Send all variables with their selected/ written value.
  function sendVar(e) {
    e.preventDefault();
    var i;
    for (i = 0; i < state.variable.length; i++) {
      var variable = state.variable[i];
      if (state.type[i] === "radio") {

        var j;
        var values = state.options[i].split(',');
        for (j = 0; j < values.length; j++) {
          var value = values[j];
          var radioButtonValue = document.getElementById(value);
          if (radioButtonValue.checked) {
            ws.send(`VSMMessage#VAR#${variable}#${value}`);
          }
        }
      } else if (state.type[i] === "text") {
        if (inputValue.has(variable)){
          ws.send(`VSMMessage#VAR#${variable}#${inputValue.get(variable)}`);
        }
      } else if (state.type[i] === "checkbox") {
        if (document.getElementById(variable).checked) {
          ws.send(`VSMMessage#VAR#${variable}#true`);
        } else {
          ws.send(`VSMMessage#VAR#${variable}#false`);
        }
      }
    }
  }

  function sendGo() {
    ws.send("VSMMessage#Go")
  }

  function inputWithType(i) {
    var variable = state.variable[i];
    if (state.type[i] === "text") {
      return (
        <>
        <p>
            <label> {state.variable[i]} </label>
            <input type="text" name={variable} placeholder={state.options[i]} id={variable} onChange={e => inputValue.set(variable, e.target.value)}/>
        </p>
        </>
      )
    } else if (state.type[i] === "radio") {
      var values = state.options[i].split(',');
      return (
        <>
        <p>
        <label> {state.variable[i]} </label>
        {values.map((option) =>
            <>
            <input type="radio" id={option} name={variable} value={option}/>
            <label> {option} </label>
            </>
        )}
        </p>
        </>
      )
    } else if (state.type[i] === "checkbox") {
      return (
          <>
          <p>
            <label> {state.variable[i]} </label>
            <input type="checkbox" id={variable} name={variable} value={state.options[i]}/>
            <label> {state.options[i]} </label>
          </p>
          </>
      )
    }
  }


  function returnAllFieldset() {
    var returnValue = [];

    var i;
    for (i = 0; i < state.variable.length; i++) {
      var inputBoxes = inputWithType(i);
      returnValue.push(inputBoxes);
    }
    return(
    <>
        {returnValue}
    </>
    )
  }


  return (
    <div className="App">
    <header className="App-header">
    {/*
       <img src={logo} className="App-logo" alt="logo" />
      <button onClick={sendGo}> Go </button>
      <form onSubmit={sendmsg}>
        <input id = 'in' />
        <button type='submit'> send </button>
        {text}
      </form>
    */}
    <form>
        <fieldset>
            {(state && (state.action === "REQUEST")) && returnAllFieldset()}
        </fieldset>
        <button onClick={sendSubmit}> submit </button>
        <button onClick={sendCancel}> cancel </button>
    </form>
    </header>
    </div>
  )
  ;
}

export default App;
