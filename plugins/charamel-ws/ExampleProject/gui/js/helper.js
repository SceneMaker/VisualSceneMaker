
function bindEvent(element, eventName, eventHandler) {
    if (element.addEventListener) {
        element.addEventListener(eventName, eventHandler, false);
    } else if (element.attachEvent) {
        element.attachEvent('on' + eventName, eventHandler);
    }
}

// Listen to messages from parent window
bindEvent(window, 'message', function (e) {
    const cmdstr = e.data.split('$');
    let element_vals = [];

    try {
        //seperating elements for setGraph and setSpeecBubble cmds
        element_vals = cmdstr[1].split('#');
    } catch (error) {
        //fallback case also used for set element=x cmds
        element_vals = cmdstr[1];
    }


    if (element_vals.length == 1) {
        element = document.getElementById(cmdstr[0]);//.contentDocument.getElementById(cmdstr[0]) //cmdstr[0]);
        // do nothing if there is no such element
        if (typeof element === 'undefined') {
            return;
        }
        // assign the value
        // element.value = cmdstr[1];
        element.value = element_vals;
    } else if (element_vals.includes('setSpeechBubble')) {
        const cmd = element_vals[0];
        const el = cmdstr[0];
        const producer = element_vals[1];
        const value = element_vals[2];
        // do nothing if there is no such element
        if (typeof el === 'undefined') {
            return;
        }
        let newDiv = document.createElement("div");
        newDiv.className = el.toString().toLowerCase() + ' ' + producer.toString().toLowerCase();
        newDiv.innerHTML = value.toString();
        let menu = document.getElementById("chat");
        menu.appendChild(newDiv);
        menu.scrollTop = newDiv.offsetHeight + newDiv.offsetTop;
    } else if (element_vals.includes('setMenuItem')) {
        const el = cmdstr[0];
        const cmd = element_vals[0];
        const id = element_vals[1];
        const value = element_vals[2];
        // do nothing if there is no such element
        if (typeof el === 'undefined') {
            return;
        }

        var button = document.createElement("button");
        button.innerHTML = value;
        button.className = "btn btn-secondary m-1 days_buttons"
        button.addEventListener("click", function () {
            parent.postMessage(id, '*');
        });

        let menu = document.getElementById("menu_items");
        menu.appendChild(button);


    } else if (element_vals.includes('setAudioItem')) {
        const el = cmdstr[0];
        const cmd = element_vals[0];
        const src = element_vals[1];
        // do nothing if there is no such element
        if (typeof el === 'undefined') {
            return;
        }

        var audio_element = document.createElement("AUDIO");
        audio_element.setAttribute("id", el);
        if (audio_element.canPlayType("audio/mpeg")) {
            audio_element.setAttribute("src", src);
        } else {
            alert("Ensure that your browser supports mpeg.");
        }

        audio_element.setAttribute("controls", "controls");
        audio_element.setAttribute("controlsList", "nodownload");

        let audio_div = document.getElementById("audio_div");
        audio_div.appendChild(audio_element);


    } else if (element_vals.includes('controlAudio')) {
        const el = cmdstr[0];
        const cmd = element_vals[0];
        const cmd_type = element_vals[1];
        // do nothing if there is no such element
        if (typeof el === 'undefined') {
            return;
        }
        const audio = document.getElementById(el);
        if (cmd_type === "stop") {
            if (audio) {
                audio.pause();
                audio.currentTime = 0;
            }
        } else if (cmd_type === "play") {
            if (audio) {
                audio.currentTime = 0;
                var playPromise = audio.play();

                // In browsers that don’t yet support this functionality,
                // playPromise won’t be defined.
                if (playPromise !== undefined) {
                    playPromise.then(function () {
                        // Automatic playback started!
                        console.log("Playback successful.")
                    }).catch(function (error) {
                        // Automatic playback failed.
                        console.log("Playback not successful. Probably requires Audio permission in the browser.")
                    });
                }
            }
        }
    }
    else if (element_vals.includes('setMoodGraph') || element_vals.includes('setWorkHrsGraph')) {
        const graph_cmd = element_vals[0];
        const day = element_vals[1];
        const type = element_vals[2];
        const value = element_vals[3];

        var element = document.getElementById(cmdstr[0]);//.contentDocument.getElementById(cmdstr[0]) //cmdstr[0]);

        // do nothing if there is no such element
        if (typeof element === 'undefined') {
            //alert("Shit!");
            return;
        }

        var weekdays = ['Mo', 'Di', 'Mi', 'Do', 'Fr', 'Sa', 'So'];
        if (graph_cmd.toUpperCase() == 'setMoodGraph'.toUpperCase()) {
            if (!sessionStorage.getItem("moodArrays")) {
                var graphs = {
                    emotion: [10, 10, 10, 10, 10, 10, 10],
                    //antrieb: [10, 10, 10, 10, 10, 10, 10],
                    //anspannung: [10, 10, 10, 10, 10, 10, 10]
                }
                sessionStorage.setItem("moodArrays", JSON.stringify(graphs));
            }
            moodarrays = JSON.parse(sessionStorage.getItem("moodArrays"));
            moodarrays[type][weekdays.indexOf(day)] = parseInt(value);
            sessionStorage.setItem("moodArrays", JSON.stringify(moodarrays));
        } else if (graph_cmd.toUpperCase() == 'setWorkHrsGraph'.toUpperCase()) {
            if (!sessionStorage.getItem("workHrsArrays")) {
                var graphs_y = {
                    inside: [0.0, 0.0, 0.0, 0.0, 0.0],
                    outside: [0.0, 0.0, 0.0, 0.0, 0.0]
                }
                sessionStorage.setItem("workHrsArrays", JSON.stringify(graphs_y));
            }
            workhrsarrays = JSON.parse(sessionStorage.getItem("workHrsArrays"));
            workhrsarrays[type][weekdays.indexOf(day)] = parseFloat(value);
            sessionStorage.setItem("workHrsArrays", JSON.stringify(workhrsarrays));
        }

    }

});
