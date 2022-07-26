const pr = window.devicePixelRatio;
const vw = window.innerWidth;
const vh = window.innerHeight;
const scaled_width = Math.round(vw);
const scaled_height = Math.round(vh);
document.getElementById("graph_arbeitszeit").style.width = scaled_height.toString();
document.getElementById("graph_arbeitszeit").style.height = scaled_width.toString();
console.log(vw, vh);
console.log(scaled_width, scaled_height);

var weekdays = ['mo', 'di', 'mi', 'do', 'fr'];

var trace1 = {
    x: weekdays,
    y: [3.9, 2.8, 4.0, 4.0, 2.0],
    name: 'Arbeitszeit im Zielsetzungsbereich',
    type: 'bar',
    marker: {
        color: 'rgb(68, 203, 163)',
    }
};

var trace2 = {
    x: weekdays,
    y: [1.1, 0.0, 2.1, 0.4, 0.0],
    name: 'Arbeitszeit ueberschreitet Zielsetzungsbereich',
    type: 'bar',
    marker: {
        color: 'rgb(38, 115, 89)',
    }
};

var data = [trace1, trace2];

var layout = {
    barmode: 'stack',
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
        "gridcolor": "white",
        "zerolinecolor": "white",
        // "zerolinewidth": 15,
    },
    paper_bgcolor: 'rgb(184, 230, 215)',
    plot_bgcolor: 'rgb(184, 230, 215)'
};

graphs = {
    inside: trace1,
    outside: trace2
}

if (!sessionStorage.getItem("workHrsArrays")) {
    var graphs_y = {
        inside: [0.0, 0.0, 0.0, 0.0, 0.0],
        outside: [0.0, 0.0, 0.0, 0.0, 0.0]
    }
    sessionStorage.setItem("workHrsArrays", JSON.stringify(graphs_y));
}
graphs_y = JSON.parse(sessionStorage.getItem("workHrsArrays"));
graphs.inside.y = graphs_y.inside;
graphs.outside.y = graphs_y.outside;

Plotly.newPlot('graph_arbeitszeit', data, layout, {displayModeBar: false});