 /**
 * This script codes for the user-interface app which samplers/interpreters use to 
 * collect the reference data which will be used to calculate map accuracy
 * and to perform design-based area estimation 
 */
 
 var geometry = /* color: #d63000 */ee.Geometry.Point([10.809665615764613, 59.92372888829894]);
 
 var aoi = geometry

var s2Col = 'S2' // or 'S2_SR' for surface reflectance
var sceneCloudThreshold = 60;
var cloudMaskProbability = 40;
var monthStart = 6;
var monthEnd = 9;
var S2_BANDS = ['QA60', 'B1','B2','B3','B4', 'B5', 'B6', 'B7','B8','B11','B12']; // Sentinel bands
var S2_NAMES = ['QA60','cb', 'blue', 'green', 'red', 'R1', 'R2', 'R3','nir','swir1', 'swir2']; // Common names


var s2_2015 = getCleanSentCollection(aoi, 2015, 2015).select(['red','green','blue']).median();
var s2_2018 = getCleanSentCollection(aoi, 2018, 2018).select(['red','green','blue']).median();
var s2_2021 = getCleanSentCollection(aoi, 2021, 2021).select(['red','green','blue']).median();


var proj = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
  .filter(ee.Filter.eq('system:index', '20150812T104021_20160413T153307_T32VNM'))
  .first().projection()

var vhr2015 = ee.ImageCollection("users/zandersamuel/NINA/Raster/Norway_VHR_2015");
var oddOneOut2015 = vhr2015.filter(ee.Filter.eq('system:index', 'IMG_SPOT6_MS_201608170948245_ORT_2859334101_R2C1')).first();
oddOneOut2015 = oddOneOut2015.select(['b3','b2','b1','b4'],['b1','b2','b3','b4'])
vhr2015 = ee.ImageCollection([oddOneOut2015]).merge(vhr2015
  .filter(ee.Filter.neq('system:index', 'IMG_SPOT6_MS_201608170948245_ORT_2859334101_R2C1')))
  .map(function(i){ return i.selfMask()})
  
var vhr2018 = ee.ImageCollection("users/zandersamuel/NINA/Raster/Norway_VHR_2018");
var oddOnesOut2018 = vhr2018.filter(ee.Filter.inList('system:index', ['IMG_SP06_NAO_MS4__3_20180721T103009_20180721T103015_TOU_1234_eb88_R1C1', 'IMG_SP06_NAO_MS4__3_20180714T103334_20180714T103338_TOU_1234_8f53_R1C1']));
oddOnesOut2018 = oddOnesOut2018.map(function(i){return i.divide(10).float()})
vhr2018 = oddOnesOut2018.merge(vhr2018
  .filter(ee.Filter.neq('system:index', 'IMG_SP06_NAO_MS4__3_20180721T103009_20180721T103015_TOU_1234_eb88_R1C1'))
  .filter(ee.Filter.neq('system:index', 'IMG_SP06_NAO_MS4__3_20180714T103334_20180714T103338_TOU_1234_8f53_R1C1')))
  .map(function(i){ return i.selfMask()})
  
var ortho2016 = ee.ImageCollection('users/zandersamuel/NINA/Raster/Oslo_kommune_orthophoto_2016_summer');
var ortho2021 = ee.ImageCollection('users/zandersamuel/NINA/Raster/Oslo_kommune_orthophoto_2021');

// pre-exported NDVI time series to make app run faster
var ndviTs = ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/ndvi_time_series');

var  ar5_col = ee.ImageCollection("users/zandersamuel/NINA/Raster/Norway_FKB_AR5_2022").select('artyp');

var ar5Tree = ar5_col.mosaic().eq(30);

function getTrendImg(){
  var dw = ee.ImageCollection("GOOGLE/DYNAMICWORLD/V1")
    .select('trees')
    .filter(ee.Filter.calendarRange(6,9,'month'))
    .filterBounds(geometry);
  
  var dwAnnual = [];                                                                   
  for (var i = 2015; i <= 2021; i++) { 
    var tmp_probs = dw.filter(ee.Filter.calendarRange(i,i, 'year')).median(); 
    dwAnnual = dwAnnual.concat(tmp_probs.set('system:time_start', (new Date(i,8,1)).valueOf())); 
  }
  dwAnnual = ee.ImageCollection(dwAnnual); 
  
  var trendGrey = getTrend(dwAnnual);
  
  return trendGrey
}

function getTrend(collection) {
  var start = ee.Date(ee.Image(collection.first()).get('system:time_start')).get('year')
  collection = collection.map(function(img){
     var year = ee.Date(img.get('system:time_start')).get('year').subtract(ee.Number(start))
    return ee.Image(year).byte().addBands(img).set('system:time_start', img.get('system:time_start'))
  });
  var trend = collection.reduce(ee.Reducer.linearFit()).select('scale');
  return trend
}

var trendTree = getTrendImg().updateMask(ar5Tree);
trendTree = trendTree.lt(-0.07).selfMask()


var SAMPLER = null;
var INDEX = 0;
var YEAR = '2015';
print(ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/Trond').size())
var samplerDict = {
  'Trond': ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/Trond').sort('PLOTID'),
  'Erik': ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/Erik').sort('PLOTID'),
  'Megan': ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/Megan').sort('PLOTID'),
  'Anders': ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/Anders').sort('PLOTID'),
  'David': ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/David').sort('PLOTID'),
  'Zander1': ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/Zander1').sort('PLOTID'),
  'Zander2': ee.FeatureCollection('projects/nina/GIS_synergy/Extent/Sampling/Sorted/Zander2').sort('PLOTID'),
}

var SQUARE_STYLE = {color: 'yellow', fillColor: '00000000', width:2};

var panelIntro = ui.Panel({style: {position: 'top-center'}});

var samplerSelect = ui.Select(Object.keys(samplerDict), 'Trond', 'Trond')

var indexSelect = ui.Textbox('0', INDEX);

var startButton = ui.Button('Start sampling', startApp);

panelIntro
  .add(ui.Label('Please select your name'))
  .add(samplerSelect)
  .add(ui.Label('Which point index would you like to start from? 🤩'))
  .add(indexSelect)
  .add(startButton);



Map.add(panelIntro);

function getBBox(geometry, proj){
  var c1 = geometry.transform(proj, 1).coordinates()
    .map(function(p) {
      return ee.Number(p).floor()
    })
  var c2 = c1.map(function(p) { return ee.Number(p).add(1) })
  var p2 =  ee.Geometry.LineString([c1, c2], proj)
  return p2.bounds()
}


function getNDVItimeSeries(plotid){
  var chart = ui.Chart.feature.byFeature(ndviTs.filter(ee.Filter.eq('PLOTID', plotid)), 'system:time_start', 'mean')


  //var chart = ui.Chart.image.series(newCollection, location, ee.Reducer.mean(), 10)
  chart = chart.setChartType('LineChart')
    .setOptions({
      height: 245,
      curveType: 'function',
      //explorer: {axis: 'vertical'},
      interpolateNulls: true,
      title: 'Change in green colour',
      vAxis: { title: 'NDVI', viewWindow: {max:1, min:0}},
      hAxis: { title: ''},
      series: { 0: {color: 'green'}}
    })
  return chart

}

function startApp(){
  
  INDEX = parseInt(indexSelect.getValue()) 
  SAMPLER = samplerSelect.getValue();
  
  //print(sampler)
  var sample = samplerDict[SAMPLER]
  
  sample = sample.map(function(ft){
    var lon = ee.Number(1e8).multiply(ee.List(ft.geometry().coordinates()).get(0))
    var lat = ee.Number(1e8).multiply(ee.List(ft.geometry().coordinates()).get(1))
    return ft.set('lon', lon, 'lat', lat)
  })
  
  var squares = sample.map(function(ft){
    return ft.set('sampler', SAMPLER).setGeometry(getBBox(ft.geometry(), proj))
  })
  print('squares ', squares)
  
  //print(squares)
  var pointsList = squares.toList(50000);
  
  //print(pointsList.size())
  
  ui.root.widgets().reset();
  var map = ui.Map().setOptions("HYBRID");
  ui.root.widgets().add(map)
  
  map.addLayer(vhr2015.mean(), {bands:['b3','b2','b1'],min:0, max:600}, 'sat 2015',0)
  map.addLayer(vhr2015.mean(), {bands:['b4','b2','b1'],min:0, max:600}, 'sat 2015 NIR',0)
  map.addLayer(ortho2016, {min:0, max:255}, 'ortho 2016',0)
  map.addLayer(vhr2018.min(), {bands:['b1','b2','b3'],min:0, max:600}, 'sat 2018',0)
  map.addLayer(vhr2018.min(), {bands:['b4','b2','b3'],min:0, max:600}, 'sat 2018 NIR',0)
  map.addLayer(ortho2021, {min:0, max:255}, 'ortho 2021',0)
  map.addLayer(trendTree, {palette:['pink']}, 'probable clear-cuts between 2015 and 2021',0)
  map.addLayer(s2_2015, {min:0, max:2000}, 'sentinel-2 RGB 2015',0)
  map.addLayer(s2_2018, {min:0, max:2000}, 'sentinel-2 RGB 2018',0)
  map.addLayer(s2_2021, {min:0, max:2000}, 'sentinel-2 RGB 2021',0)
  map.addLayer(squares.style(SQUARE_STYLE), {});
  
  
  var panel = ui.Panel({style: {position: 'bottom-right'}});
  map.add(panel);
  
  var backButton = ui.Button('Go Back', goBack);
  var finishPanel = ui.Button('Get table', finish);
  var actionpanel = ui.Panel([backButton, ui.Button('Skip', doButton)], ui.Panel.Layout.Flow('horizontal'));
  
  var ndviPanel = ui.Panel({style: {position: 'bottom-left', shown:true, width:'300px'}});
  
  map.add(ndviPanel)
  
  var indexPanel = ui.Panel();
  var tablePanel = ui.Panel({style:{shown:false}});
  
  function addQuestions(YEAR){
    panel.widgets().reset([])
    var question1 = ui.Label('What is majority class in the square?');
    var yearText = ui.Label('For: ' + String(YEAR), {color:'red'});
    var panelQuestion = ui.Panel([], ui.Panel.Layout.Flow('horizontal'));
    var panelQ1 = ui.Panel([
      ui.Button('Built', doButton),
      ui.Button('Crops', doButton), 
      ui.Button('Bare', doButton), 
      ui.Button('Grass', doButton), 
      ui.Button('Mixed', doButton)], ui.Panel.Layout.Flow('vertical'));
    var panelQ2 = ui.Panel([
      ui.Button('Shrub', doButton),
      ui.Button('Tree', doButton),
      ui.Button('Wetland', doButton),
      ui.Button('Water', doButton),
      ui.Button('SnowIce', doButton)], ui.Panel.Layout.Flow('vertical'));
    panelQuestion.add(panelQ1).add(panelQ2)
    tablePanel.style().set({shown:false})
    panel.widgets().reset([question1, yearText, panelQuestion, actionpanel, finishPanel, indexPanel, tablePanel])
  }
  
  
  
  var newFeats = ee.List([])
  var feat = null;
  function next(){
    if (YEAR == '2021'){
      INDEX = INDEX + 1
      addQuestions('2015')
      plotNDVI()
      YEAR = '2015'
    } else if (YEAR == '2018') {
      addQuestions('2021')
      YEAR = '2021'
    } else {
      addQuestions('2018')
      YEAR = '2018'
    }
    
    var countLabel = ui.Label('Sample number: ' + (INDEX))
    indexPanel.widgets().reset([countLabel]);
    
    if (YEAR == '2015'){
      feat = ee.Feature(pointsList.get(INDEX));
      print(feat)
      map.centerObject(feat, 19);
    }
    
    if (INDEX % 50 === 0 & INDEX != 0 & YEAR == '2018'){
     map.add(ui.Panel({
       widgets: [ui.Label('Please download data and restart app from last index', {color:'ff0000'})],
       style: {height: '200px'}
     }));
   }
   if (INDEX == 799){
     map.add(ui.Panel({
       widgets: [ui.Label('YOU WIN----GAME OVER!! You are finished with sampling. Please download the last batch of data. Do not proceed to index 800 otherwise the app will break.', {color:'ff0000'})],
       style: {height: '200px'}
     }));
   }
   
  }
  
  function doButton(label){
    feat = feat.set(
      'groundTruth', label.getLabel(), 
      'sample_index', INDEX,
      'year', YEAR)
    newFeats = newFeats.add(feat)
    next()
  }
  
  
  function goBack(){
    YEAR = '2015'
    addQuestions(YEAR)
    INDEX = INDEX - 1
    plotNDVI()
    var featID = ee.Feature(pointsList.get(INDEX)).get('PLOTID');
    var countLabel = ui.Label('Sample number: ' + (INDEX))
    indexPanel.widgets().reset([countLabel]);
    feat = ee.Feature(pointsList.get(INDEX));
    map.centerObject(feat, 19)
    
    newFeats = ee.FeatureCollection(newFeats).filterMetadata('PLOTID','not_equals',featID).toList(10000);
  }
  function finish(){
    var table = ui.Chart.feature.byFeature(newFeats, 'PLOTID');
    table.setChartType('Table');
    table.setOptions({allowHtml: true, pageSize: 5});
    table.style().set({stretch: 'horizontal', width: '350px', height: '150px'});
    tablePanel.style().set({shown:true})
    tablePanel.widgets().reset([table])
    
  }
  
  function plotNDVI(){
    var plot = getNDVItimeSeries(ee.Feature(pointsList.get(INDEX)).get('PLOTID'))
    ndviPanel.widgets().reset([plot])
  }
  
  addQuestions('2015')
  plotNDVI()
  var countLabel = ui.Label('Sample number: ' + (INDEX))
  //panel.widgets().add(countLabel).add(coordLabel);
  indexPanel.widgets().reset([countLabel]);
  feat = ee.Feature(pointsList.get(INDEX));
  map.centerObject(feat, 19);
}




// Function to add spectral indices to Sentinel images
function addIndices(image) {
  var ndvi = image.normalizedDifference(['nir', 'red']).rename('ndvi');
  return image.addBands(ndvi)
}

//This procedure must be used for proper processing of S2 imagery
function uniqueValues(collection,field){
    var values  =ee.Dictionary(collection.reduceColumns(ee.Reducer.frequencyHistogram(),[field]).get('histogram')).keys();
    return values;
  }
function dailyMosaics(imgs){
  //Simplify date to exclude time of day
  imgs = imgs.map(function(img){
  var d = ee.Date(img.get('system:time_start'));
  var day = d.get('day');
  var m = d.get('month');
  var y = d.get('year');
  var simpleDate = ee.Date.fromYMD(y,m,day);
  return img.set('simpleTime',simpleDate.millis());
  });
  
  //Find the unique days
  var days = uniqueValues(imgs,'simpleTime');
  
  imgs = days.map(function(d){
    d = ee.Number.parse(d);
    d = ee.Date(d);
    var t = imgs.filterDate(d,d.advance(1,'day'));
    var f = ee.Image(t.first());
    t = t.mosaic();
    t = t.set('system:time_start',d.millis());
    t = t.copyProperties(f);
    return t;
    });
    imgs = ee.ImageCollection.fromImages(imgs);
    
    return imgs;
}

function getS2_SR_CLOUD_PROBABILITY(aoi, yearStart, yearEnd) { 
  var primary = ee.ImageCollection("COPERNICUS/" + s2Col)
      .filterBounds(aoi)
      .filter(ee.Filter.calendarRange(yearStart, yearEnd, 'year'))
      .filter(ee.Filter.calendarRange(monthStart, monthEnd, 'month'))
      .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', sceneCloudThreshold)
      .select(S2_BANDS, S2_NAMES)
      .map(addIndices);
  var secondary = ee.ImageCollection("COPERNICUS/S2_CLOUD_PROBABILITY")
      .filterBounds(aoi)
      .filter(ee.Filter.calendarRange(yearStart, yearEnd, 'year'))
      .filter(ee.Filter.calendarRange(monthStart, monthEnd, 'month'));
  var innerJoined = ee.Join.inner().apply({
    primary: primary,
    secondary: secondary,
    condition: ee.Filter.equals({
      leftField: 'system:index',
      rightField: 'system:index'
    })
  });
  var mergeImageBands = function (joinResult) {
    return ee.Image(joinResult.get('primary'))
          .addBands(joinResult.get('secondary'));
  };
  var newCollection = innerJoined.map(mergeImageBands);
  return ee.ImageCollection(newCollection);
}

function maskClouds(cloudProbabilityThreshold){
  return function(_img) {
  var cloudMask = _img.select('probability').lt(cloudProbabilityThreshold);
  return _img.updateMask(cloudMask);
}}


function getCleanSentCollection(aoi, yearStart, yearEnd){
  var s2Combo = getS2_SR_CLOUD_PROBABILITY(aoi, yearStart, yearEnd);
  
  
  var s2Cleaned =  s2Combo.map(maskClouds(cloudMaskProbability))
    .sort('system:time_start');
  
  s2Cleaned = dailyMosaics(s2Cleaned); 
  
  return s2Cleaned
}