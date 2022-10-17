/**
 * Copyright 2021 Justas Lavišius <bucaneer@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

const chart_id = 'poll-timeline';
const locale = document.documentElement.lang;

// Define base chart configuration
const chart_config = {
  type: 'mixed',
  locale: locale,
  legend: {
    align: 'center',
    verticalAlign: 'top',
    tooltip: {
      text: '%plot-description',
    },
    layout: 'float',
    borderWidth: 0,
    item: {
      padding: 5,
      cursor: "pointer",
    },
  },
  scaleX: {
    transform: {
      type: 'date',
      all: '%Y-%mm-%dd',
    },
    guide: {
      visible: true,
      lineColor: '#ffffff',
    },
    lineColor: 'white',
  },
  scaleY: {
    label: { text: locale === 'lt' ? 'Populiarumas' : 'Popularity' },
    format: "%v%",
    guide: {
      visible: true,
      lineColor: '#ffffff',
    },
    minValue: 0,
    lineColor: 'white',
  },
  plot: {},
  plotarea: {
    backgroundColor: '#f5f5f5',
    marginTop: 0,
    marginBottom: 90,
    marginLeft: 55,
    marginRight: 35,
  },
  backgroundColor: '#fefefc',
  height: '600px',
  crosshairX: {
    plotLabel: {
      multiple: true,
    },
    exact: true,
    lineColor: '#302624',
    scaleLabel: {
      backgroundColor: '#302624',
    },
  },
  labels: [
    {
      text: 'https://bucaneer.github.io/lt-reitingai',
      fontColor: '#999999',
      x: 57,
      y: 490,
      zIndex: -1,
    }
  ],
  series: [],
  shapes: [],
};

// Define display styles of party data
const partyStyles = {
  "TS.LKD": {
    color: "#3359A3",
    label: "TS-LKD",
    description_lt: "Tėvynės sąjunga - Lietuvos krikščionys demokratai",
    description_en: "Homeland Union – Lithuanian Christian Democrats",
  },
  "LVZS": {
    color: "#0f7448",
    label: "LVŽS",
    description_lt: "Lietuvos valstiečių ir žaliųjų sąjunga",
    description_en: "Lithuanian Farmers and Greens Union",
  },
  "LSDP": {
    color: "#e10514",
    label: "LSDP",
    description_lt: "Lietuvos socialdemokratų partija",
    description_en: "Social Democratic Party of Lithuania",
  },
  "LRLS": {
    color: "#E69F00",
    label: "LRLS",
    description_lt: "Lietuvos Respublikos liberalų sąjūdis",
    description_en: "Liberal Movement of the Republic of Lithuania",
  },
  "LLRA.KSS": {
    color: "#009E73",
    label: "LLRA-KŠS",
    description_lt: "Lietuvos lenkų rinkimų akcija - Krikščioniškų šeimų sąjunga",
    description_en: "Electoral Action of Poles in Lithuania – Christian Families Alliance",
  },
  "TT.LT": {
    color: "#fbba00",
    label: "LT",
    description_lt: "Laisvė ir teisingumas",
    description_en: "Freedom and Justice",
  },
  "DP": {
    color: "#56B4E9",
    label: "DP",
    description_lt: "Darbo partija",
    description_en: "Labour Party",
  },
  "CP.TTS": {
    color: "#04a03c",
    label: "CP/TTS",
    description_lt: "Centro partija / Tautos ir teisingumo sąjunga",
    description_en: "Centre Party / Nation and Justice Union",
  },
  "LP": {
    color: "#ff306e",
    label: "LP",
    description_lt: "Laivės partija",
    description_en: "Freedom Party",
  },
  "LSDDP.LRP": {
    color: "#bf1e37",
    label: "LSDDP/LRP",
    description_lt: "Lietuvos socialdemokratų darbo partija / Lietuvos regionų partija",
    description_en: "Social Democratic Labour Party of Lithuania / Lithuanian Party of Regions",
  },
  "VL": {
    color: "#002060",
    label: "DSVL",
    description_lt: "Demokratų sąjunga \"Vardan Lietuvos\"",
    description_en: "Union of Democrats \"For Lithuania\"",
  },
};

// Define display styles of pollster data
const pollsterStyles = {
  "Baltijos tyrimai": {
    shape: "circle",
  },
  "Spinter": {
    shape: "square",
  },
  "Vilmorus": {
    shape: "diamond",
  },
}

const loaded_parties = [];
const dates = [];
const pollsters = [];
const hidden_parties = [];
const party_max_y = {};
const dblclick_threshhold = 500;
let polls_enabled = false;
let legend_click_target;
let legend_click_time;
let dblclick_timeout;

function loadPollModel(data) {
  const header = data[0];
  const series = {};
  const series_names = ['est', 'lower', 'upper'];
  
  // Process CSV data into data series
  data.forEach((row, i) => {
    if (i === 0) return;
    const entry = Object.assign(...header.map((k, i) => ({[k]: row[i]})));
    
    if (!series[entry['party']]) {
      series[entry['party']] = {
        est: [],
        lower: [],
        upper: [],
        change: [],
      }
    }
    
    // Populate dates (x-scale) array
    const parsed_date = +new Date(entry['date']);
    if (parsed_date != dates[dates.length-1]) {
      dates.push(parsed_date);
    }

    // Populate monthly change series
    const prev_est = (series[entry['party']]['est'][series[entry['party']]['est'].length - 4] || (entry['est']*100));
    const change = (entry['est']*100).toFixed(1) - prev_est.toFixed(1);
    series[entry['party']]['change'].push(parseFloat(change.toFixed(1)));

    // Populate main series
    series_names.forEach(name => series[entry['party']][name].push(parseFloat(entry[name])*100));

    if (!party_max_y[entry['party']] || entry['upper'] > party_max_y[entry['party']]) {
      party_max_y[entry['party']] = entry['upper'];
    }
  });

  // Sort parties by latest popularity ranking
  const latest_index = series[Object.keys(series)[0]]['est'].length - 1;
  const parties = Object.keys(series)
    .sort((a, b) => series[b]['est'][latest_index] - series[a]['est'][latest_index]);

  parties.forEach(party => {
    if (party === 'oth') return;

    const range = series[party]['lower'].map((low, i) => {
      return [low, series[party]['upper'][i]];
    });

    loaded_parties.push(party);

    // Add confidence range plot
    chart_config.series.push({
      id: party + "-range",
      text: party + "-range",
      values: range.map((value, i) => [dates[i], value]),
      type: 'range',
      lineWidth: '0px',
      marker: {
        visible: false
      },
      tooltip: {
        visible: false
      },
      legendItem: {
        visible: false
      },
      guideLabel: {
        visible: false
      },
      alphaArea: 0.05,
      backgroundColor: partyStyles[party]['color'],
      decimals: 1,
      zIndex: -1,
    });

    // Add monthly change plot
    chart_config.series.push({
      id: party + "-change",
      text: party + "-change",
      values: series[party]['change'].map((value, i) => [dates[i], value]),
      type: 'line',
      visible: false,
      legendItem: {
        visible: false,
      },
      guideLabel: {
        visible: false,
      },
      marker: {
        visible: false,
      },
      tooltip: {
        visible: false,
      },
      decimals: 1,
      zIndex: -1,
    });

    // Add estimation plot
    chart_config.series.push({
      id: party,
      text: partyStyles[party]['label'],
      description: partyStyles[party]['description_'+locale],
      values: series[party]['est'].map((value, i) => [dates[i], value]),
      type: 'line',
      marker: {
        visible: false,
      },
      guideLabel: {
        backgroundColor: partyStyles[party]['color'],
        borderWidth: 0,
        borderRadius: 5,
        padding: 5,
        text: '%t: <b>%v%</b> (%plot-'+(chart_config.series.length-1) + '-value/'+(locale == 'lt' ? 'mėn.' : 'month') + ')',
        fontColor: 'white',
      },
      tooltip: {
        visible: false,
      },
      legendItem: {
        backgroundColor: partyStyles[party]['color'],
        borderRadius: 5,
        fontColor: "white",
        shadow: false,
        borderWidth: 0,
      },
      legendMarker: {
        visible: false,
      },
      lineColor: partyStyles[party]['color'],
      lineWidth: '3px',
      decimals: 1,
      zIndex: 5,
    });
  });

  chart_config.scaleX.step = dates[1] - dates[0];
  chart_config.scaleX.minValue = dates[0];
}

async function loadPolls(data) {
  const header = data[0].map(name => name.replace(/[\-\/]/, '.'));
  const series = {};

  // Wait until all relevant variables from the model data are initialized
  await window.model_promise;

  const min_date = dates[0];
  const max_date = dates[dates.length - 1];

  data.forEach((row, i) => {
    if (i === 0) return;
    const entry = Object.assign(...header.map((k, i) => ({[k]: row[i]})));
    
    // Calculate middle of the polling period
    const date = +new Date(entry['start_date']) + (+new Date(entry['end_date']) - new Date(entry['start_date']))/2;
    if (date < min_date || date > max_date) return;

    // Find the best-fitting position in the dates array
    const date_index = Math.max(0, Math.floor((date - min_date) / chart_config.scaleX.step) - 1);
    const adj_date = dates[date_index];

    if (!pollsters.includes(entry['pollster'])) {
      pollsters.push(entry['pollster']);
    }

    // Populate pollster/party data series
    loaded_parties.forEach(party => {
      const series_id = party + '-' + entry['pollster'];
      if (!series[entry['pollster']]) {
        series[entry['pollster']] = {};
      }

      if (!series[entry['pollster']][party]) {
        series[entry['pollster']][party] = new Array(dates.length).fill(null);
      }

      series[entry['pollster']][party].push([adj_date, parseFloat(entry[party]) * 100]);

      if (entry[party] > party_max_y[party]) {
        party_max_y[party] = entry[party];
      }
    });
  });

  // Only keep polls from pollsters with 3 or more polls
  pollsters.splice(
    0,
    pollsters.length,
    ...pollsters
      .filter(pollster => {
        return series[pollster][loaded_parties[0]].filter(x => x !== null).length >= 3;
      })
      .sort()
  );

  // Add toggle button behind pollster legend
  chart_config.shapes.push({
    id: 'toggle-polls',
    type: "rect",
    y: 560,
    x: 60 + (70*pollsters.length)/2,
    width: 75 * pollsters.length,
    height: 35,
    backgroundColor: 'transparent',
    borderWidth: 1,
    borderColor: 'black',
    cursor: 'pointer',
  });

  pollsters.forEach((pollster, p_i) => {
    // Add pollster/party poll plot
    loaded_parties.forEach(party => {
      chart_config.series.push({
        id: party + '-poll-' + pollster,
        text: partyStyles[party]['label'] + ' (' +pollster + ')',
        type: 'scatter',
        values: series[pollster][party],
        color: partyStyles[party]['color'],
        decimals: 1,
        tooltip: {
          visible: false,
          text: "%t: %v%",
          backgroundColor: partyStyles[party]['color'],
          shadow: false,
          borderWidth: 0,
        },
        legendItem: {
          visible: false
        },
        guideLabel: {
          visible: false,
        },
        marker: {
          borderColor: partyStyles[party]['color'],
          borderWidth: 2,
          backgroundColor: 'transparent',
          type: pollsterStyles[pollster]['shape'],
          size: 3,
          visible: polls_enabled,
        },
        zIndex: 2,
        anchor: 'c',
      });
    });

    // Add pollster shape legend
    chart_config.shapes.push({
      type: pollsterStyles[pollster]['shape'],
      id: pollster + '-legend',
      size: 5,
      borderWidth: 2,
      backgroundColor: "transparent",
      borderColor: "black",
      x: 95 + (70 * p_i),
      y: 555,
      label: {
        text: pollster,
        offsetY: 15,
      },
    });

  });
  
  renderChart();
}

function togglePlot(party, hide) {
  // Add or remove party ID from hidden_parties list
  if (hide) {
    if (!hidden_parties.includes(party)) {
      hidden_parties.push(party);
    }
  } else {
    if (hidden_parties.includes(party)) {
      hidden_parties.splice(hidden_parties.indexOf(party), 1);
    }
  }

  // Toggle estimation plot visibility
  zingchart.exec(chart_id, 'modifyplot', {
    plotid: party,
    data: { visible: !hide },
    update: false
  });
  
  
  // Toggle confidence range plot visibility
  zingchart.exec(chart_id, 'modifyplot', {
    plotid: party+'-range',
    data: { alphaArea: hide ? 0 : 0.05 },
    update: false
  });

  // Toggle poll plot visibility for this party
  pollsters.forEach(pollster => {
    zingchart.exec(chart_id, 'modifyplot', {
      plotid: party+'-poll-'+pollster,
      data: { marker: { visible: (polls_enabled && !hide) } },
      update: false,
    });
  });
}

function onLegendClick(e) {
  clearTimeout(dblclick_timeout);

  const prev_click_time = legend_click_time;
  const prev_click_target = legend_click_target;
  legend_click_time = e.ev.timeStamp;
  legend_click_target = e.plotid;

  const is_dblclick = !e.visible
    && prev_click_target === legend_click_target
    && legend_click_time < prev_click_time + dblclick_threshhold;

  togglePlot(e.plotid, e.visible);

  // Handle double-click event
  if (is_dblclick) {
    // If only the clicked party is currently visible, make all others visible;
    // otherwise, hide all others and leave only the clicked party visible.
    const hide_state = hidden_parties.length !== loaded_parties.length - 1;
    loaded_parties.forEach(party => {
      if (party !== e.plotid) togglePlot(party, hide_state);
    });
  }
  
  // Set Y scale max to the next multiple of 2% over the maximum visible plot point
  const y_max = 2 * Math.ceil(
    100 * Math.max(
      ...loaded_parties.filter(x => !hidden_parties.includes(x))
        .map(x => party_max_y[x])
    ) / 2
  );
  zingchart.exec(chart_id, 'modify', { update: false, data: { scaleY: { maxValue: y_max } } });

  dblclick_timeout = setTimeout(()=>zingchart.exec(chart_id,'update'), is_dblclick ? 1 : dblclick_threshhold);
};

function onShapeClick(e) {
  if (e.shapeid === 'toggle-polls') {
    polls_enabled = !polls_enabled;
    
    // Toggle poll plot visibility for all parties
    pollsters.forEach(pollster => {
      loaded_parties.forEach(party => {
        zingchart.exec(chart_id, 'modifyplot', {
          plotid: party+'-poll-'+pollster,
          data: {marker: {visible: (polls_enabled && !hidden_parties.includes(party) && !e.visible)}},
          update: false,
        });
      });
    });

    setTimeout(()=>zingchart.exec(chart_id,'update'),0);
  }
}

function renderChart() {
  zingchart.bind(chart_id, 'legend_item_click', onLegendClick);
  zingchart.bind(chart_id, 'legend_marker_click', onLegendClick);
  zingchart.bind(chart_id, 'shape_click', onShapeClick);

  zingchart.render({
    id: chart_id,
    data: chart_config,
    height: '600px',
  });
}

window.addEventListener('load', async (event) => {
  window.model_promise = fetch('_output/model-latest.csv', {cache: 'no-cache'})
    .then(response => response.text())
    .then(data => loadPollModel(data.csvToArray({rSep:"\n"})) );

  window.polls_promise = fetch('polls.csv', {cache: 'no-cache'})
    .then(response => response.text())
    .then(data => loadPolls(data.csvToArray({rSep:"\n"})) );
});
