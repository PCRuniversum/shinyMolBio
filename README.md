# shinyMolBio
Molecular biology visualization tools for Shiny apps

<div>
<style type="text/css">table.pcr-plate-tbl{  width: 100%;  border-collapse: separate;  border-spacing: 1px;}table.pcr-plate-tbl td, table.pcr-plate-tbl th{  width: 4rem;  height: 2rem;  border: 2px solid #ccc;  text-align: center;}table.pcr-plate-tbl td{  max-width: 4rem;  word-wrap: break-word;}table.pcr-plate-tbl th{  border-color: white;  color: white;}table.pcr-plate-tbl thead th:nth-child(even){  background-color: #3CA9E8;}table.pcr-plate-tbl thead th:nth-child(odd){  background-color: #178ACC;}th.odd-row{  background-color: #3CA9E8;}th.even-row{  background-color: #178ACC;}td.selected-well{  border: 2px solid black !important;}th.toggle-all {  background: transparent !important;  position: relative;}th.toggle-all:after {    content: "";    position: absolute;    bottom: 0;    right: 0;	width: 0;	height: 0;    display: block;	border-left: 1em solid transparent;	border-bottom: 1em solid transparent;	border-bottom: 1em solid grey;}#customLabel td.selected-well{border: 2px solid red !important;}
               #customLabel .ntc{border: 3px solid Plum;}
               #customLabel .unkn{border: 3px solid Salmon;}
               #customLabel .pos{border: 3px solid PaleGreen ;}
               #customLabel .neg{border: 3px solid LightGrey ;}
               #customLabel .std{border: 3px solid DeepSkyBlue ;}
               #customLabel .filled-circle1 {padding: 2px 11px;
                  border-radius: 100%; background-color: Maroon;}
               #customLabel .filled-circle2 {padding: 2px 11px;
                  border-radius: 100%; background-color: Orange;}</style>
<div id="customLabel" class="pcr-plate">
  <label for="customLabel">Custom labels and marks</label>
  <table id="customLabel-pcrPlateTbl" class="pcr-plate-tbl"><thead><tr><th id="customLabel-toggleall" class="toggle-all"></th><th id='customLabel-col_01'>1</th><th id='customLabel-col_02'>2</th><th id='customLabel-col_03'>3</th><th id='customLabel-col_04'>4</th><th id='customLabel-col_05'>5</th><th id='customLabel-col_06'>6</th><th id='customLabel-col_07'>7</th><th id='customLabel-col_08'>8</th><th id='customLabel-col_09'>9</th><th id='customLabel-col_10'>10</th><th id='customLabel-col_11'>11</th><th id='customLabel-col_12'>12</th></tr></thead><tbody><tr><th id='customLabel-row_A' class='odd-row'>A</th><td id='A01' title='RNase P: FAM' group='NTCRNase PRNase P' class='ntc' style='background-color:rgba(76,0,255,0.2);'><span class='filled-circle2'></span>NTC_RNase P Cq=40</td><td id='A02' title='RNase P: FAM' group='NTCRNase PRNase P' class='ntc' style='background-color:rgba(76,0,255,0.2);'><span class='filled-circle2'></span>NTC_RNase P Cq=40</td><td id='A03' title='RNase P: FAM' group='NTCRNase PRNase P' class='ntc' style='background-color:rgba(76,0,255,0.2);'><span class='filled-circle2'></span>NTC_RNase P Cq=40</td><td id='A04' title='RNase P: FAM' group='pop1RNase PRNase P' class='unkn' style='background-color:rgba(0,25,255,0.2);'><span class='filled-circle1'></span>pop1_RNase P Cq=29</td><td id='A05' title='RNase P: FAM' group='pop1RNase PRNase P' class='unkn' style='background-color:rgba(0,25,255,0.2);'><span class='filled-circle1'></span>pop1_RNase P Cq=29</td><td id='A06' title='RNase P: FAM' group='pop1RNase PRNase P' class='unkn' style='background-color:rgba(0,25,255,0.2);'><span class='filled-circle1'></span>pop1_RNase P Cq=29</td><td id='A07' title='RNase P: FAM' group='pop2RNase PRNase P' class='unkn' style='background-color:rgba(0,128,255,0.2);'><span class='filled-circle1'></span>pop2_RNase P Cq=28</td><td id='A08' title='RNase P: FAM' group='pop2RNase PRNase P' class='unkn' style='background-color:rgba(0,128,255,0.2);'><span class='filled-circle1'></span>pop2_RNase P Cq=28</td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td></tr><tr><th id='customLabel-row_B' class='even-row'>B</th><td id='B01' title='RNase P: FAM' group='pop2RNase PRNase P' class='unkn' style='background-color:rgba(0,128,255,0.2);'><span class='filled-circle1'></span>pop2_RNase P Cq=28</td><td id='B02' title='RNase P: FAM' group='STDRNase P100000RNase P' class='std' style='background-color:rgba(0,229,255,0.2);'><span class='filled-circle1'></span>STD_RNase P_10000.0 Cq=27</td><td id='B03' title='RNase P: FAM' group='STDRNase P100000RNase P' class='std' style='background-color:rgba(0,229,255,0.2);'><span class='filled-circle1'></span>STD_RNase P_10000.0 Cq=27</td><td id='B04' title='RNase P: FAM' group='STDRNase P100000RNase P' class='std' style='background-color:rgba(0,229,255,0.2);'><span class='filled-circle1'></span>STD_RNase P_10000.0 Cq=27</td><td id='B05' title='RNase P: FAM' group='STDRNase P50000RNase P' class='std' style='background-color:rgba(0,255,77,0.2);'><span class='filled-circle1'></span>STD_RNase P_5000.0 Cq=28</td><td id='B06' title='RNase P: FAM' group='STDRNase P50000RNase P' class='std' style='background-color:rgba(0,255,77,0.2);'><span class='filled-circle1'></span>STD_RNase P_5000.0 Cq=28</td><td id='B07' title='RNase P: FAM' group='STDRNase P50000RNase P' class='std' style='background-color:rgba(0,255,77,0.2);'><span class='filled-circle1'></span>STD_RNase P_5000.0 Cq=28</td><td id='B08' title='RNase P: FAM' group='STDRNase P25000RNase P' class='std' style='background-color:rgba(230,255,0,0.2);'><span class='filled-circle1'></span>STD_RNase P_2500.0 Cq=29</td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td></tr><tr><th id='customLabel-row_C' class='odd-row'>C</th><td id='C01' title='RNase P: FAM' group='STDRNase P25000RNase P' class='std' style='background-color:rgba(230,255,0,0.2);'><span class='filled-circle1'></span>STD_RNase P_2500.0 Cq=29</td><td id='C02' title='RNase P: FAM' group='STDRNase P25000RNase P' class='std' style='background-color:rgba(230,255,0,0.2);'><span class='filled-circle1'></span>STD_RNase P_2500.0 Cq=29</td><td id='C03' title='RNase P: FAM' group='STDRNase P12500RNase P' class='std' style='background-color:rgba(255,255,0,0.2);'>STD_RNase P_1250.0 Cq=30</td><td id='C04' title='RNase P: FAM' group='STDRNase P12500RNase P' class='std' style='background-color:rgba(255,255,0,0.2);'>STD_RNase P_1250.0 Cq=30</td><td id='C05' title='RNase P: FAM' group='STDRNase P12500RNase P' class='std' style='background-color:rgba(255,255,0,0.2);'>STD_RNase P_1250.0 Cq=30</td><td id='C06' title='RNase P: FAM' group='STDRNase P6250RNase P' class='std' style='background-color:rgba(255,224,179,0.2);'>STD_RNase P_625.0 Cq=31</td><td id='C07' title='RNase P: FAM' group='STDRNase P6250RNase P' class='std' style='background-color:rgba(255,224,179,0.2);'>STD_RNase P_625.0 Cq=31</td><td id='C08' title='RNase P: FAM' group='STDRNase P6250RNase P' class='std' style='background-color:rgba(255,224,179,0.2);'>STD_RNase P_625.0 Cq=31</td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td></tr><tr><th id='customLabel-row_D' class='even-row'>D</th><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td></tr><tr><th id='customLabel-row_E' class='odd-row'>E</th><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td></tr><tr><th id='customLabel-row_F' class='even-row'>F</th><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td></tr><tr><th id='customLabel-row_G' class='odd-row'>G</th><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td></tr><tr><th id='customLabel-row_H' class='even-row'>H</th><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td><td class='empty-well'></td></tr></tbody></table>
  <div>
    <span class="filled-circle1"></span>
    Cq &lt; 30
    <br/>
    <span class="filled-circle2"></span>
    Cq &gt; 35
    <br/>
    <span>
      <span class="ntc">NTC</span>
      <span class="unkn">Unknown</span>
      <span class="pos">Positive</span>
      <span class="neg">Negative</span>
      <span class="std">Standard</span>
    </span>
  </div>
</div>
</div>
