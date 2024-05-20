const pr = window.devicePixelRatio;
const vw = window.innerWidth;
const vh = window.innerHeight;
const scaled_width = Math.round(vw);
const scaled_height = Math.round(vh);
document.getElementById("graph_stimmungsbarometer").style.width = scaled_height.toString();
document.getElementById("graph_stimmungsbarometer").style.height = scaled_width.toString();
console.log(vw, vh);
console.log(scaled_width, scaled_height);

var weekdays = ['Mo', 'Di', 'Mi', 'Do', 'Fr', 'Sa', 'So'];

var trace1 = {
    x: weekdays,
    y: [10, 15, 13, 17, 13, 9, 11],
    mode: 'lines',
    name: 'emotion',
    marker: {color: 'rgb(38, 115, 89)'},
    // line: {width: 20}
};

var trace2 = {
    x: weekdays,
    y: [12, 17, 15, 19, 12, 7, 12],
    mode: 'lines',
    name: 'antrieb',
    marker: {color: 'rgb(68, 203, 163)'},
    // line: {width: 20}
};

var trace3 = {
    x: weekdays,
    y: [8, 11, 9, 13, 19, 18, 13],
    mode: 'lines',
    name: 'anspannung',
    marker: {color: 'rgb(80, 243, 194)'},
    // line: {width: 20}
};

//var data = [trace1, trace2, trace3];
var data = [trace1];

var layout = {
    title: 'Stimmungsbarometer',
    showlegend: true,
    legend: {
        xanchor: 'center',
        x: 0.5,
        orientation: 'h',
        y: -0.15
    },
    font: {
        // size: 40,
    },
    yaxis: {
        showgrid: true,
        // "gridwidth": 15,
        // "zerolinewidth": 15,
    },
    paper_bgcolor: 'rgb(184, 230, 215)',
    plot_bgcolor: 'rgb(184, 230, 215)'
};

var graphs = {
    emotion: trace1
    //antrieb: trace2,
    //anspannung: trace3
}

if (!sessionStorage.getItem("moodArrays")) {
    var graphs_y = {
        emotion: [10, 10, 10, 10, 10, 10, 10],
        //antrieb: [10, 10, 10, 10, 10, 10, 10],
        //anspannung: [10, 10, 10, 10, 10, 10, 10]
    }
    sessionStorage.setItem("moodArrays", JSON.stringify(graphs_y));
}
graphs_y = JSON.parse(sessionStorage.getItem("moodArrays"));
graphs.emotion.y = graphs_y.emotion;
//graphs.antrieb.y = graphs_y.antrieb;
//graphs.anspannung.y = graphs_y.anspannung;

Plotly.newPlot('graph_stimmungsbarometer', data, layout, {displayModeBar: false});
