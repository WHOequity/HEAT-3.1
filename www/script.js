
// © Copyright World Health Organization (WHO) 2016.
// This file is part of the Health Equity Assessment Toolkit (HEAT).
// HEAT is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License Version 2 as published by
// the Free Software Foundation.

// HEAT is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
// You should have received a copy of the GNU General Public License
// along with HEAT. If not, see http://www.gnu.org/licenses/.

var justifyColumns = function (chart) {
    var categoriesWidth = chart.plotSizeX / (1 + chart.xAxis[0].max - chart.xAxis[0].min),
        distanceBetweenColumns = 0,
        each = Highcharts.each,
        sum, categories = chart.xAxis[0].categories,
        number;
    for (var i = 0; i < categories.length; i++) {
        sum = 0;
        each(chart.series, function (p, k) {
            if (p.visible) {
                each(p.data, function (ob, j) {
                    if (ob.category == categories[i]) {
                        sum++;
                    }
                });
            }
        });
        distanceBetweenColumns = categoriesWidth / (sum + 1);
        number = 1;
        each(chart.series, function (p, k) {
            if (p.visible) {
                each(p.data, function (ob, j) {
                    if (ob.category == categories[i]) {
                        ob.graphic.element.x.baseVal.value = i * categoriesWidth + distanceBetweenColumns * number - ob.pointWidth / 2;
                        number++;
                    }
                });
            }
        });
    }
};


$(document).ready(function() {
  $('[data-toggle="tooltip"]').tooltip({
      placement : 'top'
  });

  //http://stackoverflow.com/questions/14248194/close-responsive-navbar-automatically
  $(".navbar-collapse a").click(function() {
    $(".navbar-collapse").collapse('hide');
  });

  $("#hc_model_compare").on("hidden.bs.modal", function(e) {
    $("#zoomhc_compare").children().remove();
  });
  
  $("#hc_model_explore").on("hidden.bs.modal", function(e) {
    $("#zoomhc_explore").children().remove();
  });
  
  /*var detailDrag = false;
  var scrollTarget;
  var detailCharts;
  $("#disag_plot_explore_dtl")
    .on("shiny:recalculated", function(e) {
      detailCharts = $(e.delegateTarget)
        .find(".highchart")
        .map(function(i, el) {
          return $(el).highcharts();
        })
        .get();
    })
    .on("mousedown", ".chartbox .highcharts-scrollbar g:first-of-type", function(e) {
      detailDrag = true;
      scrollTarget = e.currentTarget;
    })
    .on("mousemove", function(e) {
      if (detailDrag) {
        let scrolls = $(e.delegateTarget).find(".chartbox .highcharts-scrollbar g:first-of-type");
        
        scrolls.attr(
          "transform", 
          scrollTarget.attributes.transform.value
        );
        
        detailCharts.forEach(function($el) {
          $el.trigger("render");
        });
      }
    })
    .on("mouseup", function(e) {
      detailDrag = false;
    });*/

});


