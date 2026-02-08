
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
    var pulsingHeart = document.getElementById("pulsingheart");

    pulsingHeart.addEventListener("touchstart", function(event) {
        event.stopPropagation();
    });

    // check if the command is 'setQuestionText'
    if (cmdstr[0] == 'setQuestionText') {
    // Select all elements with the particular IDs (even though IDs should be unique)
        var elements = document.querySelectorAll('#QuestionDisplay, #OptionsDisplay, #QuestionText, #head');

        // Log the number of elements found
        console.log(`Found ${elements.length} elements`);

        // Loop through each element and show it by setting its display to 'block' or an empty string
        elements.forEach((element, index) => {
            console.log(`Showing element ${index + 1}`);
            element.style.display = ''; // or 'block', depending on the element
        });

        const text = cmdstr[1];  // extract the text from cmdstr
        // get the element from the DOM
        let element = document.getElementById('QuestionDisplay');
        // set the inner HTML of the element to the received text
        if (element) {
            element.innerHTML = text;
        } else {
           console.log("Element with id 'QuestionDisplay' not found");
        }
    }

    if (cmdstr[0] == 'setOptionsText') {
        const text = cmdstr[1];
        let words = text.split(' ');
        let newText = '';
        let isFirstWord = true;
        let counter = 0;

        for (let word of words) {
            if (isFirstWord) {
                newText += '<span class="title">' + word; // Include the title in the same line
                isFirstWord = false;
            } else {
                if (!isNaN(word.charAt(0))) {
                    if(counter == 0){
                        newText += '<br></span>';
                    }
                    newText += '<span class="numbered-option">';
                    newText += '<br>' + '<span class="option-number">' + word + ' ' + '</span>';
                 counter++;
                } else {
                newText += word + ' ';
                }

            }
        }
        newText += '<br>';
        let element = document.getElementById('OptionsDisplay');
        element.style.display = 'block';
        if (element) {
          element.innerHTML = newText;
        } else {
            console.log("Element with id 'OptionsDisplay' not found");
        }
    }

    if (cmdstr[0] == 'refreshGui') {
        // Select all elements with the particular IDs (even though IDs should be unique)
        var elements = document.querySelectorAll('#QuestionDisplay, #OptionsDisplay, #QuestionText, #head');

        // Log the number of elements found
        console.log(`Found ${elements.length} elements`);

        // Loop through each element and hide it by setting its display to 'none'
        elements.forEach((element, index) => {
            console.log(`Hiding element ${index + 1}`);
         element.style.display = 'none';
         });
    }

    if(cmdstr[0] == 'selectTime'){
        // Time selector code
        const hoursSelect = document.getElementById('hours');
        const minutesSelect = document.getElementById('minutes');
        const secondsSelect = document.getElementById('seconds');
        const showTimeButton = document.getElementById('showTimeButton');
        const selectedTime = document.getElementById('selectedTime');

        // Populate hours dropdown
        for (let i = 0; i < 24; i++) {
            let option = document.createElement('option');
            option.value = i;
            option.text = i < 10 ? '0' + i : i;
            hoursSelect.appendChild(option);
        }

        // Populate minutes and seconds dropdowns
        for (let i = 0; i < 60; i++) {
            let option = document.createElement('option');
            option.value = i;
            option.text = i < 10 ? '0' + i : i;
            minutesSelect.appendChild(option);

            option = document.createElement('option');
            option.value = i;
            option.text = i < 10 ? '0' + i : i;
            secondsSelect.appendChild(option);
        }

        // Show selected time when button is clicked
        showTimeButton.addEventListener('click', function () {
            const hours = hoursSelect.value;
            const minutes = minutesSelect.value;
            const seconds = secondsSelect.value;
            selectedTime.textContent = `Selected Time: ${hours}:${minutes}:${seconds}`;
        });
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
    } else if (element_vals.includes('showElement')) { // shows a gui element
        const el = cmdstr[0];
        // do nothing if there is no such element
        if (typeof el === 'undefined') {
            return;
        }
        const cmd = element_vals[0];
        const id = element_vals[1];
        document.getElementById(id).style.display = "block";
    } else if (element_vals.includes('hideElement')) { // hide a gui element
        const el = cmdstr[0];
        // do nothing if there is no such element
        if (typeof el === 'undefined') {
            return;
        }
        const cmd = element_vals[0];
        const id = element_vals[1];
        document.getElementById(id).style.display = "none";
    } else if (element_vals.includes('muteMic')) { // mute the microphone
//        videoMediaStream.getAudioTracks()[0].enabled = false;
        console.log("Mic is muted");
        emma.muteMicrophone(true);
    } else if (element_vals.includes('openMic')) { // open the microphone
//        videoMediaStream.getAudioTracks()[0].enabled = false
        console.log("Mic is open");
        emma.muteMicrophone(false);
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
        // do nothing if there is no such element
        if (typeof el === 'undefined') {
            return;
        }
        const cmd = element_vals[0];
        const id = element_vals[1];
        const value = element_vals[2];
        const type = element_vals[3];
        var button = document.createElement("button");
        button.innerHTML = value;
        if (typeof type === 'undefined') {
            button.className = "btn btn-secondary m-1 days_buttons";
        } else {
            // any value in type will use the important_buttons style
            button.className = "btn btn-secondary m-1 important_buttons";
        }

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
                        console.log("Playback not successful. Probably requires audio permission in the browser.")
                    });
                }

                // adding event listeners
                audio.addEventListener("ended", function() {
                  parent.postMessage('audio_playback_ended', '*');
                  console.log("Playback ended.")
                });

                audio.addEventListener("pause", function() {
                  parent.postMessage('audio_playback_pause', '*');
                  console.log("Playback paused.")
                });

                audio.addEventListener("play", function() {
                  parent.postMessage('audio_playback_play', '*');
                  console.log("Playback play.")
                });

            }
        }
    } else if (element_vals.includes('setMoodGraph') || element_vals.includes('setWorkHrsGraph')) {
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
