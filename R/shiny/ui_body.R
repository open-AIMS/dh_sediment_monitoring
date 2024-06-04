source("shiny/ui_body_landing.R")
source("shiny/ui_body_dashboard.R")
source("shiny/ui_body_data.R")
source("shiny/ui_body_eda.R")
source("shiny/ui_body_analysis.R")
source("shiny/ui_body_manual.R")


tag_styles <- tags$style(HTML(
  '
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left .control-label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .form-control {
      flex-basis: 300px;          /* Target width for slider */
    }

    .fa-circle-check {
      color: green; 
    }
        
    .fa-clock {
      color: orange;
    }

    .fa-circle-exclamation {
      color: orange;
    }

    .fa-circle-xmark {
      color: red;
    }

    #log_output {
      height: 200px;
      overflow-y: auto;
      display:flex;
      flex-direction: column-reverse;
    }
    #model_log_output {
      height: 200px;
      overflow-y: auto;
      display:flex;
      flex-direction: column-reverse;
    }

    .callout {
  border-left: 3px solid #BBB;
  background-color: #EEE;
  display: block;
  position: relative;
  overflow: auto;
  }

  .call-info {
  border: 1px solid #0288D1;
  background-color: #D3EFFF;
  font-size: 17px;
  border-left: 3px solid #0288D1;
}

.callout h4 {
  color: black;
font-weight: 500;
}
.call-info h4::before {
  content: "🛈" ;
  color: #0288D1;
  padding-right: 10px;
  vertical-align: middle;
  display: inline-block;

}

details {
margin-top: -10px;
}
details[open] {
  border: 1px solid #0288D1;
  background-color: #D3EFFF;
  border-left: 3px solid #0288D1;
  padding-left:5px;

}

ul {
padding-bottom:10px;
}

  //.details-info {
  //border: 1px solid #0288D1;
  //background-color: #D3EFFF;
  //border-left: 3px solid #0288D1;
  //padding-left:5px;
//}

summary {
 display:list-item;
}

.table-minimal table {
  border: 2px solid #000000;
  width: 100%;
  text-align: left;
  border-collapse: collapse;
}
.table-minimal td, .table-minimal th {
  border: 1px solid #000000;
  padding: 5px 4px;
}
.table-minimal tbody td {
  font-size: 13px;
}
.table-minimal thead {
  background: #D0E4F5;
  border-bottom: 1px solid #000000;
}
.table-minimal thead th {
  font-size: 15px;
  font-weight: bold;
  color: #000000;
  text-align: left;
  border-left: 1px solid #D0E4F5;
}
.table-minimal thead th:first-child {
  border-left: none;
}

.table-minimal tfoot td {
  font-size: 14px;
}

// buttons
.bttn[disabled] {
 cursor: not-allowed;
 color:grey;
 border-color:grey;
 background-color: white;
}

.btn-default {
   background-color: #1d89ff;
   color: #fff;
   border-color: #1d89ff;
}
.btn-default:hover {
   background-color: #fff;
   color: #1d89ff;
   border-color: #1d89ff;
}

.btn-disabled {
   background-color: #888;
   color: #000;
   border-color: #888;
}

.btn-disabled:hover {
   background-color: #888;
   color: #000;
   border-color: #888;
}
.btn-success {
   background-color: #00a65a;
   color: #fff;
   border-color: #00a65a;
}

.btn-success .fa-circle-check {
   color: #fff;
}

.btn-success:hover {
   background-color: #00a65a;
   color: #fff;
   border-color: #00a65a;
}

.btn-danger {
   background-color: #ab0f0e;
   color: #fff;
   border-color: #ab0f0e;
}

.btn-danger .fa-circle-check {
   color: #fff;
}

.btn-danger:hover {
   background-color: #ab0f0e;
   color: #fff;
   border-color: #ab0f0e;
}
.btn-warning {
   background-color: #cf4d03;
   color: #fff;
   border-color: #cf4d03;
}

.btn-warning .fa-circle-check {
   color: #fff;
}

.btn-warning:hover {
   background-color: #cf4d03;
   color: #fff;
   border-color: #cf4d03;
}
    '
)) 


## callout_style <- tags$style(HTML(
## "
## /*
## $positive: #0f7d15;
## $negative: #ab0f0e;
## $info: #0288D1;
## $warning: #cf4d03;
## $positive-dark: #4aa850;
## $negative-dark: #e85c5b;
## $info-dark: #0288D1;
## $warning-dark: #de8a5a;
## */
## .callout {
##   border-left: 3px solid #BBB;
##   background-color: #EEE;
##   padding: $-s $-s $-s $-xl;
##   display: block;
##   position: relative;
##   overflow: auto;
## &:before {
##     background-image: url('data:image/svg+xml;base64,PHN2ZyB2aWV3Qm94PSIwIDAgMjQgMjQiIGZpbGw9IiMwMTUzODAiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+ICAgIDxwYXRoIGQ9Ik0wIDBoMjR2MjRIMHoiIGZpbGw9Im5vbmUiLz4gICAgPHBhdGggZD0iTTEyIDJDNi40OCAyIDIgNi40OCAyIDEyczQuNDggMTAgMTAgMTAgMTAtNC40OCAxMC0xMFMxNy41MiAyIDEyIDJ6bTEgMTVoLTJ2LTZoMnY2em0wLThoLTJWN2gydjJ6Ii8+PC9zdmc+');
##     background-repeat: no-repeat;
##     content: '';
##     width: 1.2em;
##     height: 1.2em;
##     left: $-xs + 2px;
##     top: 50%;
##     margin-top: -9px;
##     display: inline-block;
##     position: absolute;
##     line-height: 1;
##     opacity: 0.8;
##   }
##   &.success {
##     @include lightDark(border-left-color, $positive, $positive-dark);
##     @include lightDark(background-color, lighten($positive, 68%), darken($positive-dark, 36%));
##     @include lightDark(color, darken($positive, 16%), $positive-dark);
##   }
##   &.success:before {
##     background-image: url('data:image/svg+xml;base64,PHN2ZyB2aWV3Qm94PSIwIDAgMjQgMjQiIGZpbGw9IiMzNzZjMzkiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+ICAgIDxwYXRoIGQ9Ik0wIDBoMjR2MjRIMHoiIGZpbGw9Im5vbmUiLz4gICAgPHBhdGggZD0iTTEyIDJDNi40OCAyIDIgNi40OCAyIDEyczQuNDggMTAgMTAgMTAgMTAtNC40OCAxMC0xMFMxNy41MiAyIDEyIDJ6bS0yIDE1bC01LTUgMS40MS0xLjQxTDEwIDE0LjE3bDcuNTktNy41OUwxOSA4bC05IDl6Ii8+PC9zdmc+');
##   }
##   &.danger {
##     @include lightDark(border-left-color, $negative, $negative-dark);
##     @include lightDark(background-color, lighten($negative, 56%), darken($negative-dark, 55%));
##     @include lightDark(color, darken($negative, 20%), $negative-dark);
##   }
##   &.danger:before {
##     background-image: url('data:image/svg+xml;base64,PHN2ZyB2aWV3Qm94PSIwIDAgMjQgMjQiIGZpbGw9IiNiOTE4MTgiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+ICAgIDxwYXRoIGQ9Ik0xNS43MyAzSDguMjdMMyA4LjI3djcuNDZMOC4yNyAyMWg3LjQ2TDIxIDE1LjczVjguMjdMMTUuNzMgM3pNMTIgMTcuM2MtLjcyIDAtMS4zLS41OC0xLjMtMS4zIDAtLjcyLjU4LTEuMyAxLjMtMS4zLjcyIDAgMS4zLjU4IDEuMyAxLjMgMCAuNzItLjU4IDEuMy0xLjMgMS4zem0xLTQuM2gtMlY3aDJ2NnoiLz4gICAgPHBhdGggZD0iTTAgMGgyNHYyNEgweiIgZmlsbD0ibm9uZSIvPjwvc3ZnPg==');
##   }
##   &.info {
##     ## @include lightDark(border-left-color, $info, $info-dark);
##     ## @include lightDark(color, darken($info, 20%), $info-dark);
##     ## @include lightDark(background-color, lighten($info, 50%), darken($info-dark, 34%));
## background-color: $info-dark;
##   }
##   &.warning {
##     @include lightDark(border-left-color, $warning, $warning-dark);
##     @include lightDark(background-color, lighten($warning, 50%), darken($warning-dark, 50%));
##     @include lightDark(color, darken($warning, 20%), $warning-dark);
##   }
##   &.warning:before {
##     background-image: url('data:image/svg+xml;base64,PHN2ZyB2aWV3Qm94PSIwIDAgMjQgMjQiIGZpbGw9IiNiNjUzMWMiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+ICAgIDxwYXRoIGQ9Ik0wIDBoMjR2MjRIMHoiIGZpbGw9Im5vbmUiLz4gICAgPHBhdGggZD0iTTEgMjFoMjJMMTIgMiAxIDIxem0xMi0zaC0ydi0yaDJ2MnptMC00aC0ydi00aDJ2NHoiLz48L3N2Zz4=');
##   }
##   a {
##     color: inherit;
##     text-decoration: underline;
##   }
## }
## "
## ))


body <- dashboardBody(
        tag_styles,
        # callout_style,
        tabItems(
                ## Settings tab
                ## settings_tab,
                ## Dashboard tab
                landing_tab,
                dashboard_tab,
                ## Data tab
                data_tab,
                ## EDA
                eda_tab,
                analysis_tab,
                manual_tab
        )
)
