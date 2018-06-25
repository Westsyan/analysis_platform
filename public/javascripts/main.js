/**
 * Created by yz on 2017/6/20.
 */
var html=" "

function linear(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    var parameter = $("#line").data("url");
    $.ajax({
        url: url + "?" + parameter,
        type: "get",
        dataType: "json",
        success: function (data) {
            var category = data.maxCategory;
            xLen = category.length;
            step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
            $("#charts").highcharts({
                plotOptions: {
                    line: {
                        turboThreshold: 0 //不限制数据点个数
                    }
                },
                chart: {
                    plotBorderWidth: 1,
                    zoomType: 'x',
                    height: 550,
                    marginTop: 40,
                    marginBottom: 170,
                    events: {
                        //监听图表区域选择事件
                        selection: function (event) {//动态修改
                            var len = event.xAxis[0].max - event.xAxis[0].min;
                            var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                            DynamicChangeTickInterval(interval);
                        }
                    }
                },
                credits: {
                    enabled: false
                },
                //标题
                title: {
                    text: null
                },
                //x轴
                xAxis: {
                    type: 'category',
                    categories: category,
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    tickInterval: step,
                    title: {
                        text: 'Samples'
                    }
                },
                yAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    min: 0,
                    title: {
                        text: 'Expression (FPKM)'
                    },
                    plotLines: [{
                        value: 0,
                        width: 1,
                        color: '#808080'
                    }]
                },
                legend: {
                    layout: 'vertical',
                    align: 'left',
                    verticalAlign: 'middle',
                    borderWidth: 0
                },
                exporting:{
                    sourceWidth: 1200,
                    sourceHeight: 800
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal',
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '0px',
                            top: '450px',
                            textAlign: 'center'
                        },
                    }],

                },
                //取的数据
                series: data.infos
            }, function (chartObj) {
                //获得图表对象
                chart = chartObj;
            });
        }
    });
}

function linearCh(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    var parameter = $("#line").data("url");
    $.ajax({
        url: url + "?" + parameter,
        type: "get",
        dataType: "json",
        success: function (data) {
            var category = data.maxCategory;
            xLen = category.length;
            step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
            $("#charts").highcharts({
                plotOptions: {
                    line: {
                        turboThreshold: 0 //不限制数据点个数
                    }
                },
                chart: {
                    plotBorderWidth: 1,
                    zoomType: 'x',
                    height: 550,
                    marginTop: 40,
                    marginBottom: 170,
                    events: {
                        //监听图表区域选择事件
                        selection: function (event) {//动态修改
                            var len = event.xAxis[0].max - event.xAxis[0].min;
                            var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                            DynamicChangeTickInterval(interval);
                        }
                    }
                },
                credits: {
                    enabled: false
                },
                //标题
                title: {
                    text: null
                },
                //x轴
                xAxis: {
                    type: 'category',
                    categories: category,
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    tickInterval: step,
                    title: {
                        text: '样品名'
                    }
                },
                yAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    min: 0,
                    title: {
                        text: '表达量 (FPKM)'
                    },
                    plotLines: [{
                        value: 0,
                        width: 1,
                        color: '#808080'
                    }]
                },
                legend: {
                    layout: 'vertical',
                    align: 'left',
                    verticalAlign: 'middle',
                    borderWidth: 0
                },
                exporting:{
                    sourceWidth: 1200,
                    sourceHeight: 800
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal',
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '0px',
                            top: '450px',
                            textAlign: 'center'
                        },
                    }],

                },
                //取的数据
                series: data.infos
            }, function (chartObj) {
                //获得图表对象
                chart = chartObj;
            });
        }
    });
}

function boxPlot(id, group1,group2,path) {
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    var url = path + "?id=" + id + "&group1=" + group1 + "&group2=" + group2 + "";
    $.ajax({
        url: url,
        type: "get",
        dataType: "json",
        success: function (data) {
            $("#charts").highcharts({
                credits: {
                    enabled: false
                },
                chart: {
                    type: 'boxplot',
                    height: 550,
                    marginTop: 40,
                    marginBottom: 80,
                    marginLeft:300,
                    marginRight:300
                },
                title: {
                    text: id
                },
                legend: {
                    enabled: false
                },
                xAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    categories: data.tissue,
                    title: {
                        text: 'Groups '
                    }
                },
                yAxis: {
                    title: {
                        text: 'Expression (FPKM)'
                    },
                    min: 0
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal'
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '90px',
                            top: '475px'
                        }
                    }]
                },
                exporting:{
                    sourceWidth: 1200,
                    sourceHeight: 800
                },
                plotOptions: {
                    boxplot: {
                        fillColor: '#F0F0E0',
                        lineWidth: 2,
                        medianColor: '#0C5DA5',
                        medianWidth: 3,
                        stemColor: '#A63400',
                        stemDashStyle: 'dot',
                        stemWidth: 1,
                        whiskerColor: '#3D9200',
                        whiskerLength: '20%',
                        whiskerWidth: 3
                    }
                },
                series: [{
                    name: id,
                    data: data.ev
                },
                    {
                        name: 'Outlier',
                        color: Highcharts.getOptions().colors[0],
                        type: 'scatter',
                        marker: {
                            fillColor: 'white',
                            lineWidth: 1,
                            lineColor: Highcharts.getOptions().colors[0]
                        },
                        tooltip: {
                            pointFormat: 'RPM: {point.y}'
                        }
                    }]
            });
        }
    });
}

function boxPlotCh(id, group1,group2,path) {
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    var url = path + "?id=" + id + "&group1=" + group1 + "&group2=" + group2 + "";
    $.ajax({
        url: url,
        type: "get",
        dataType: "json",
        success: function (data) {
            $("#charts").highcharts({
                credits: {
                    enabled: false
                },
                chart: {
                    type: 'boxplot',
                    height: 550,
                    marginTop: 40,
                    marginBottom: 80,
                    marginLeft:300,
                    marginRight:300
                },
                title: {
                    text: id
                },
                legend: {
                    enabled: false
                },
                xAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    categories: ["分组1","分组2"]
                    ,
                },
                yAxis: {
                    title: {
                        text: '表达量 (FPKM)'
                    },
                    min: 0
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal'
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '90px',
                            top: '475px'
                        }
                    }]
                },
                exporting:{
                    sourceWidth: 1200,
                    sourceHeight: 800
                },
                plotOptions: {
                    boxplot: {
                        fillColor: '#F0F0E0',
                        lineWidth: 2,
                        medianColor: '#0C5DA5',
                        medianWidth: 3,
                        stemColor: '#A63400',
                        stemDashStyle: 'dot',
                        stemWidth: 1,
                        whiskerColor: '#3D9200',
                        whiskerLength: '20%',
                        whiskerWidth: 3
                    }
                },
                tooltip: {
                    pointFormat: '<span style="color:{point.color}">\u25CF</span> <b> {series.name}</b><br/>' +
                    '最大值: {point.high}<br/>' +
                    'Q2\t: {point.q3}<br/>' +
                    '中位数: {point.median}<br/>' +
                    'Q1\t: {point.q1}<br/>' +
                    '最小值: {point.low}<br/>'
                },
                series: [{
                    name: id,
                    data: data.ev
                },
                    {
                        name: 'Outlier',
                        color: Highcharts.getOptions().colors[0],
                        type: 'scatter',
                        marker: {
                            fillColor: 'white',
                            lineWidth: 1,
                            lineColor: Highcharts.getOptions().colors[0]
                        },
                        tooltip: {
                            pointFormat: 'FPKM: {point.y}'
                        }
                    }]
            });
        }
    });
}

function heatmap(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    parameter = $("#heatmap").data("url");
    $.ajax({
        url: url + "?" + parameter,
        type: "get",
        dataType: "json",
        success: function (data) {
            xLen = data[0].treatment.length;
            step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
            $("#charts").highcharts({
                plotOptions: {
                    heatmap: {
                        turboThreshold: 0 //不限制数据点个数
                    }
                },
                credits: {
                    enabled: false
                },
                chart: {
                    zoomType: 'xy',
                    type: 'heatmap',
                    height: 550,
                    marginTop: 65,
                    marginBottom: 170,
                    events: {
                        //监听图表区域选择事件
                        selection: function (event) {//动态修改
                            var len = event.xAxis[0].max - event.xAxis[0].min;
                            var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                            DynamicChangeTickInterval(interval);
                        }
                    }
                },
                title: {
                    text: null
                },
                xAxis: {
                    categories: data[0].treatment,
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    tickInterval: step,
                    title: {
                        text: 'Samples'
                    }
                },
                exporting:{
                    sourceWidth: 1200,
                    sourceHeight: 800
                },
                yAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    categories: data[0].gt,
                    title: "Gene"
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal'
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '0px',
                            top: '425px'
                        }
                    }]
                },
                colorAxis: {
                    stops: [
                        [0.1, '#78b8ed'],
                        [0.5, '#fffbbc'],
                        [0.8, '#c4463a'],
                        [1, '#c4463a']
                    ],
                    min: 0
//                        minColor: '#FFFFFF'
//                        maxColor: Highcharts.getOptions().colors[0]
                },
                legend: {
                    title: {
                        style: {
                            fontWeight: '1',
                            color: '#555',
                            fontSize: '12px'
                        },
                        text: 'FPKM (log2+1)'
                    },
                    align: 'right',
                    layout: 'vertical',
                    marginTop: 0,
                    verticalAlign: 'top',
                    y: 25,
                    symbolHeight: 305
                },
                tooltip: {
                    formatter: function () {
                        return '<b>Sample Name:</b>' + this.series.xAxis.categories[this.point.x] + '<br>' + this.series.yAxis.categories[this.point.y] + '<br><b>FPKM (log2+1): ' + this.point.value + '</b>';
                    }
                },
                series: [{
                    borderWidth: '0.2',
                    color: '#fefefe',
                    data: data[0].expression,
                    dataLabels: {
                        enabled: false
                    }
                }]
            }, function (chartObj) {
                //获得图表对象
                chart = chartObj;
            });

        }
    });
}

function heatmapCh(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    parameter = $("#heatmap").data("url");
    $.ajax({
        url: url + "?" + parameter,
        type: "get",
        dataType: "json",
        success: function (data) {
            xLen = data[0].treatment.length;
            step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
            $("#charts").highcharts({
                plotOptions: {
                    heatmap: {
                        turboThreshold: 0 //不限制数据点个数
                    }
                },
                credits: {
                    enabled: false
                },
                chart: {
                    zoomType: 'xy',
                    type: 'heatmap',
                    height: 550,
                    marginTop: 65,
                    marginBottom: 170,
                    events: {
                        //监听图表区域选择事件
                        selection: function (event) {//动态修改
                            var len = event.xAxis[0].max - event.xAxis[0].min;
                            var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                            DynamicChangeTickInterval(interval);
                        }
                    }
                },
                title: {
                    text: null
                },
                xAxis: {
                    categories: data[0].treatment,
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    tickInterval: step,
                    title: {
                        text: '样品'
                    }
                },
                yAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    categories: data[0].gt,
                    title: "Gene"
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal'
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '0px',
                            top: '425px'
                        }
                    }]
                },
                colorAxis: {
                    stops: [
                        [0.1, '#78b8ed'],
                        [0.5, '#fffbbc'],
                        [0.8, '#c4463a'],
                        [1, '#c4463a']
                    ],
                    min: 0
//                        minColor: '#FFFFFF'
//                        maxColor: Highcharts.getOptions().colors[0]
                },
                legend: {
                    title: {
                        style: {
                            fontWeight: '1',
                            color: '#555',
                            fontSize: '12px'
                        },
                        text: 'FPKM (log2+1)'
                    },
                    align: 'right',
                    layout: 'vertical',
                    marginTop: 0,
                    verticalAlign: 'top',
                    y: 25,
                    symbolHeight: 305
                },
                exporting:{
                    sourceWidth: 1200,
                    sourceHeight: 800
                },
                tooltip: {
                    formatter: function () {
                        return '<b>样品名:</b>' + this.series.xAxis.categories[this.point.x] + '<br>' + this.series.yAxis.categories[this.point.y] + '<br><b>FPKM (log2+1): ' + this.point.value + '</b>';
                    }
                },
                series: [{
                    borderWidth: '0.2',
                    color: '#fefefe',
                    data: data[0].expression,
                    dataLabels: {
                        enabled: false
                    }
                }]
            }, function (chartObj) {
                //获得图表对象
                chart = chartObj;
            });

        }
    });
}

function cheatmap(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    $.ajax({
        //  url: url + "?" + parameter,
        url: "/SAGC/analyse/heatmap",
        type: "get",
        dataType: "json",
        data: $("#conditionForm").serialize(),
        success: function (data) {
            if (data.valid == "false") {
                swal("Error", data.message, "error")
                $("#result").hide()
            } else {
                xLen = data[0].treatment.length;
                step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
                $("#charts").highcharts({
                    plotOptions: {
                        heatmap: {
                            turboThreshold: 0 //不限制数据点个数
                        }
                    },
                    credits: {
                        enabled: false
                    },
                    chart: {
                        zoomType: 'xy',
                        type: 'heatmap',
                        height: 750,
                        marginTop: 65,
                        marginBottom: 170,
                        marginLeft: 300,
                        marginRight: 300,
                        events: {
                            //监听图表区域选择事件
                            selection: function (event) {//动态修改
                                var len = event.xAxis[0].max - event.xAxis[0].min;
                                var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                                DynamicChangeTickInterval(interval);
                            }
                        }
                    },
                    title: {
                        text: null
                    },
                    xAxis: {
                        categories: data[0].treatment,
                        labels: {
                            rotation: -45,
                            style: {
                                fontSize: '12px',
                                fontFamily: 'Arial, sans-serif'
                            }
                        },
                        tickInterval: step,
                        title: {
                            text: 'Correlation Heatmap'
                        }
                    },
                    exporting:{
                        sourceWidth: 1200,
                        sourceHeight: 800
                    },
                    yAxis: {
                        labels: {
                            style: {
                                fontSize: '12px',
                                fontFamily: 'Arial, sans-serif'
                            }
                        },
                        categories: data[0].gt,
                        title: "Gene"
                    },
                    labels: {
                        style: {
                            color: "black",
                            fontSize: '16px',
                            fontWeight: 'normal'
                        },
                        items: [{
                            html: html,
                            style: {
                                left: '0px',
                                top: '425px'
                            }
                        }]
                    },
                    colorAxis: {
                        stops: [
                            [0.1, '#78b8ed'],
                            [0.5, '#fffbbc'],
                            [0.8, '#c4463a'],
                            [1, '#c4463a']
                        ],
                        min: data[0].min,
                        max: data[0].max
//                        minColor: '#FFFFFF'
//                        maxColor: Highcharts.getOptions().colors[0]
                    },
                    legend: {
                        title: {
                            style: {
                                fontWeight: '1',
                                color: '#555',
                                fontSize: '12px'
                            },
                            text: 'C.C'
                        },
                        align: 'right',
                        layout: 'vertical',
                        marginTop: 0,
                        verticalAlign: 'top',
                        y: 25,
                        symbolHeight: 505
                    },
                    tooltip: {
                        formatter: function () {
                            return '<b>C.C: ' + this.point.value + '</b>';
                        }
                    },
                    series: [{
                        borderWidth: '0.2',
                        color: '#fefefe',
                        data: data[0].expression,
                        dataLabels: {
                            enabled: false
                        }
                    }]
                }, function (chartObj) {
                    //获得图表对象
                    chart = chartObj;
                });
            }
        }
    });
}

function cheatmapCh(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    $.ajax({
        //  url: url + "?" + parameter,
        url: "/SAGC/analyse/heatmap",
        type: "get",
        dataType: "json",
        data: $("#conditionForm").serialize(),
        success: function (data) {
            if (data.valid == "false") {
                swal({
                    title: "错误!",
                    text: data.message,
                    type: "error",
                    confirmButtonText: "确定"
                })
                $("#result").hide()
            } else {
                xLen = data[0].treatment.length;
                step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
                $("#charts").highcharts({
                    plotOptions: {
                        heatmap: {
                            turboThreshold: 0 //不限制数据点个数
                        }
                    },
                    credits: {
                        enabled: false
                    },
                    chart: {
                        zoomType: 'xy',
                        type: 'heatmap',
                        height: 750,
                        marginTop: 65,
                        marginBottom: 170,
                        marginLeft: 300,
                        marginRight: 300,
                        events: {
                            //监听图表区域选择事件
                            selection: function (event) {//动态修改
                                var len = event.xAxis[0].max - event.xAxis[0].min;
                                var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                                DynamicChangeTickInterval(interval);
                            }
                        }
                    },
                    title: {
                        text: null
                    },
                    xAxis: {
                        categories: data[0].treatment,
                        labels: {
                            rotation: -45,
                            style: {
                                fontSize: '12px',
                                fontFamily: 'Arial, sans-serif'
                            }
                        },
                        tickInterval: step,
                        title: {
                            text: '相关性热图'
                        }
                    },
                    yAxis: {
                        labels: {
                            style: {
                                fontSize: '12px',
                                fontFamily: 'Arial, sans-serif'
                            }
                        },
                        categories: data[0].gt,
                        title: "Gene"
                    },
                    exporting:{
                        sourceWidth: 1200,
                        sourceHeight: 800
                    },
                    labels: {
                        style: {
                            color: "black",
                            fontSize: '16px',
                            fontWeight: 'normal'
                        },
                        items: [{
                            html: html,
                            style: {
                                left: '0px',
                                top: '425px'
                            }
                        }]
                    },
                    colorAxis: {
                        stops: [
                            [0.1, '#78b8ed'],
                            [0.5, '#fffbbc'],
                            [0.8, '#c4463a'],
                            [1, '#c4463a']
                        ],
                        min: data[0].min,
                        max: data[0].max
//                        minColor: '#FFFFFF'
//                        maxColor: Highcharts.getOptions().colors[0]
                    },
                    legend: {
                        title: {
                            style: {
                                fontWeight: '1',
                                color: '#555',
                                fontSize: '12px'
                            },
                            text: 'C.C'
                        },
                        align: 'right',
                        layout: 'vertical',
                        marginTop: 0,
                        verticalAlign: 'top',
                        y: 25,
                        symbolHeight: 505
                    },
                    tooltip: {
                        formatter: function () {
                            return '<b>相关性系数: ' + this.point.value + '</b>';
                        }
                    },
                    series: [{
                        borderWidth: '0.2',
                        color: '#fefefe',
                        data: data[0].expression,
                        dataLabels: {
                            enabled: false
                        }
                    }]
                }, function (chartObj) {
                    //获得图表对象
                    chart = chartObj;
                });
            }
        }
    });
}

function DynamicChangeTickInterval(step) {
    chart.xAxis[0].update({
        tickInterval: step
    });
    //扩展或者重写Highcharts图表组件的方法
    ExtendHighcharts();
}

//扩展或者重写Highcharts图表组件的方法
function ExtendHighcharts() {
    Highcharts.extend(Highcharts.Chart.prototype, {
        zoomOut: function () {
            //还原图表X轴的间隔
            DynamicChangeTickInterval(step);
            //还原图表初始状态
            this.zoom();
        }
    });
}

function cpm(obj,title) {
    var html = "    <div id=\"updateSuccess\" class=\"modal fade modal-margin\" tabindex=\"-1\" role=\"dialog\" aria-labelledby=\"myModalLabel\" aria-hidden=\"true\" style=\"margin-top: 200px;\">\n" +
        "        <div class=\"modal-dialog\" style=\"width: 400px;\">\n" +
        "            <div class=\"modal-content\">\n" +
        "                <div class=\"modal-header bg-primary\">\n" +
        "                    <h4 class=\"modal-title\" align=\"center\" id=\"successTitle\">\n" +
        "                        <span id=\"deleteTitle\" style=\"font-weight: bold;\">"+ title +"</span>\n" +
        "                    </h4>\n" +
        "                </div>\n" +
        "                <div class=\"row-fluid\" align=\"center\" >\n" +
        "                    <div id=\"successIcon\">\n" +
        "                        <img src=\"/assets/images/success.png\">\n" +
        "                    </div>\n" +
        "                </div>\n" +
        "                <div class=\"modal-footer bg-info\">\n" +
        "                    <div align=\"center\">\n" +
        "                        <button type=\"button\" class=\"btn green\" onclick=\"sureSuccess(this)\" style=\"width: 100px;\" id=\"successBtn\" value='"+ obj+"'>确定</button>\n" +
        "                    </div>\n" +
        "                </div>\n" +
        "            </div>\n" +
        "        </div>\n" +
        "    </div>";

    $(".page-content").append(html);
    $("#updateSuccess").modal("show");

}

function sureSuccess(obj) {
    var state = obj.value;
    $("#updateSuccess").modal("hide");
    console.log(state)
    if(state == "table"){
        updateTable();
    }else{
        location.reload();
    }
}

function deleteBefore() {
    $("#title1").show();
    $("#title2").hide();
    $("#title3").hide();
    $("#warn1").show();
    $("#warn2").hide();
    $("#warn3").hide();
    $("#btn1").show();
    $("#btn2").show();
    $("#btn3").hide();
}

function deleting() {
    $("#title1").hide();
    $("#title3").hide();
    $("#title2").show();
    $("#warn1").hide();
    $("#warn3").hide();
    $("#warn2").show();
    $("#btn1").hide();
    $("#btn2").hide();
    $("#btn3").hide();
}

function deleteAfter() {
    $("#title1").hide();
    $("#title2").hide();
    $("#title3").show();
    $("#warn1").hide();
    $("#warn2").hide();
    $("#warn3").show();
    $("#btn1").hide();
    $("#btn2").hide();
    $("#btn3").show();
}

function getDisk() {
    $.ajax({
        url:"/unix/getDisk",
        type: "get",
        dataType:"json",
        success: function (data) {
            $(".pb-value").width( data.per +'%');
            $(".pb-text").html(data.per +'%');
            $(".disk-text").html("已用 "+data.use+" / 总共 " + data.all)
        }
    })
}
