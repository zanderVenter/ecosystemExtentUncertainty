/**
 * This script creates stratified samples using the functions from AREA2 https://area2.readthedocs.io/en/latest/sampling_str.html
 * The script needs to be run twice (each time commenting out the irrelevant functions):
 * 1. First to get a sample size distribution estimate across classes using the getSampleSizeRecommendation()
 * 2. Then enter those sample sizes into the getStratSample_change() function to export CSV with samples
 * While running the above, the script will also perform pixel counting and built a list called strataAreas
 * which contains the pixel counting area estimates for each change map type.
 * The script exports:
 * a. CSV files with samples for samplers to label in the sampling app
 * b. CSV file of strataAreas to be fed into the area estimator in R
 * c. GeoTIFF ecosystem extent maps for visualization in QGIS
 * 
 */

/*
  // Setup global objects ///////////////////////////////////////////////////////////////////////////
*/
var geometry = /* color: #d63000 */ee.Geometry.Point([10.798600896859107, 59.963936100519625]);

Map.setOptions('HYBRID')
var CLASS_NAMES = [
    'water', 'trees', 'grass', 'flooded_vegetation', 'crops',
    'shrub_and_scrub', 'built', 'bare', 'snow_and_ice'];

var lcDict = {
  labels: [
    "Build area", //1
    "Crops", //2
    "Bare ground", //3
    "Grass", //4
    "Shrub & scrub", //5
    "Trees", //6
    "Flooded vegetation", //7
    "Water", //8
    "Snow & ice" //9
    ],
  colors: [
    '#C4281B',
    '#E49635',
    '#A59B8F',
    '#88B053',
    '#DFC35A',
    '#397D49',
    '#f2bac1', // '#7A87C6'
    '#0002f6', // '#419BDF'
    '#6fdbde' // #B39FE1'
  ]
}
var lcVizParams = {
  min: 1,
  max: 9,
  palette: lcDict['colors']
};

var StrataAreas = ee.FeatureCollection([])

var kommuner = ee.FeatureCollection('users/zandersamuel/NINA/Vector/Norway_administrative_kommuner_2022')
var aoi = kommuner.filter(ee.Filter.inList('navn', ['Oslo'])).geometry()
Map.addLayer(aoi, {}, 'aoi',0)
print(aoi.area())


/*
  // Dynamic World stratified change samples ///////////////////////////////////////////////////////////////////////////
*/

var dw = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
  .filterBounds(aoi)
  .filter(ee.Filter.calendarRange(4,9,'month'))
  .select(CLASS_NAMES);
  
var proj = dw.first().projection();
print(proj, 'projection')
print(proj.nominalScale(), 'projection scale (m)')

var lc2015 = dw
  .filter(ee.Filter.calendarRange(2015,2015,'year'))
  .reduce(ee.Reducer.median())
  .rename(CLASS_NAMES);
var lc2015Cat = probToCat_8cat(lc2015)
//Map.addLayer(lc2015Cat, lcVizParams, 'lc2015Cat dw',0);

var lc2018 = dw
  .filter(ee.Filter.calendarRange(2018,2018,'year'))
  .reduce(ee.Reducer.median())
  .rename(CLASS_NAMES);
var lc2018Cat = probToCat_8cat(lc2018)
//Map.addLayer(lc2018Cat, lcVizParams, 'lc2018Cat dw',0);

var lc2021 = dw
  .filter(ee.Filter.calendarRange(2021,2021,'year'))
  .reduce(ee.Reducer.median())
  .rename(CLASS_NAMES);
var lc2021Cat = probToCat_8cat(lc2021)
Map.addLayer(lc2021Cat, lcVizParams, 'lc2021Cat dw',0);

var lc2015_4cat = probToCat_4cat(changeTypology_4cat(lc2015));
Map.addLayer(lc2015_4cat, {min:0, max:3, palette:['black','red','green','blue']}, 'lc2015_4cat dw',0)
var lc2018_4cat = probToCat_4cat(changeTypology_4cat(lc2018));
Map.addLayer(lc2018_4cat, {min:0, max:3, palette:['black','red','green','blue']}, 'lc2018_4cat dw',0)
var lc2021_4cat = probToCat_4cat(changeTypology_4cat(lc2021));
Map.addLayer(lc2021_4cat, {min:0, max:3, palette:['black','red','green','blue']}, 'lc2021_4cat dw',0)

var lcChange1_dw = getChangeImg_4cat(lc2015_4cat, lc2018_4cat);
Map.addLayer(lcChange1_dw.randomVisualizer(), {}, 'lcChange1 dw',0)
var lcChange2_dw = getChangeImg_4cat(lc2018_4cat, lc2021_4cat);
Map.addLayer(lcChange2_dw.randomVisualizer(), {}, 'lcChange2 dw',0)
var lcChange3_dw = getChangeImg_4cat(lc2015_4cat, lc2021_4cat);
Map.addLayer(lcChange3_dw.randomVisualizer(), {}, 'lcChange3 dw',0)


Export.image.toDrive({
  image: lc2021Cat.clip(aoi),
  description: 'lc2021Cat_dw',
  region: aoi,
  scale: 10,
  crs: 'EPSG:32632'
})
Export.image.toDrive({
  image: lcChange3_dw.clip(aoi),
  description: 'lcChange2015to2021Cat_dw',
  region: aoi,
  scale: 10,
  crs: 'EPSG:32632'
})
 
getSampleSizeRecommendation(lcChange1_dw, aoi, [
  0.8, 0.6, 0.6, 0.6, 
  0.6, 0.8, 0.6, 0.6, 
  0.6, 0.6, 0.8, 0.6, 
  0.6, 0.6, 0.6, 0.8], 0.02, 'sample_oslo_change_dw_2015_2018')
  
getStratSample_change(lcChange1_dw, probToCat_8cat(lc2015),probToCat_8cat(lc2018),   aoi, 
  [ 1,2,3,4,
    5,6,7,8,
    9,10,11,12,
    13,14,15,16], 
  [60, 25, 25, 25, 
  25, 144, 25, 25, 
  25, 25, 370, 25, 
  25, 25, 25, 25],
  'sample_oslo_change_dw_2015_2018',  123, 234)


getSampleSizeRecommendation(lcChange2_dw, aoi, [
  0.8, 0.6, 0.6, 0.6, 
  0.6, 0.8, 0.6, 0.6, 
  0.6, 0.6, 0.8, 0.6, 
  0.6, 0.6, 0.6, 0.8], 0.02, 'sample_oslo_change_dw_2018_2021')

getStratSample_change(lcChange2_dw, probToCat_8cat(lc2018),probToCat_8cat(lc2021), aoi, 
  [1,2,3,4,
    5,6,7,8,
    9,10,11,12,
    13,14,15,16], 
  [41, 25, 25, 25, 
  25, 102, 25, 25, 
  25, 25, 246, 25, 
  25, 25, 25, 25],
  'sample_oslo_change_dw_2018_2021',  123, 234)
  

getSampleSizeRecommendation(lcChange3_dw, aoi, [
  0.6, 0.6, 0.6, 0.6, 
  0.6, 0.6, 0.6, 0.6, 
  0.6, 0.6, 0.6, 0.6, 
  0.6, 0.6, 0.6, 0.6], 0.02, 'sample_oslo_change_dw_2015_2021')

getStratSample_change(lcChange3_dw,probToCat_8cat(lc2015),probToCat_8cat(lc2021),  aoi, 
  [1,2,3,4,
    5,6,7,8,
    9,10,11,12,
    13,14,15,16], 
  [40, 25, 25, 25, 
  25, 98, 25, 25, 
  25, 25, 249, 25, 
  25, 25, 25, 25],
  'sample_oslo_change_dw_2015_2021',  123, 234)


/*
  // Custom RF model stratified change samples ///////////////////////////////////////////////////////////////////////////
*/

var me = ee.ImageCollection('projects/nina/GIS_synergy/Extent/rf_predictions_oslo');
Map.addLayer(me, {}, 'me raw',0)

var lc2015 = me
  .filter(ee.Filter.eq('year', 2015)).first().reproject(proj);
var lc2015Cat = probToCat_8cat(lc2015)
Map.addLayer(lc2015Cat, lcVizParams, 'lc2015Cat me',0);

var lc2018 = me
  .filter(ee.Filter.eq('year', 2018)).first().reproject(proj);
var lc2018Cat = probToCat_8cat(lc2018)
Map.addLayer(lc2018Cat, lcVizParams, 'lc2018Cat me',0);

var lc2021 = me
  .filter(ee.Filter.eq('year', 2021)).first().reproject(proj);
var lc2021Cat = probToCat_8cat(lc2021)
Map.addLayer(lc2021Cat, lcVizParams, 'lc2021Cat me',0);

var lc2015_4cat = probToCat_4cat(changeTypology_4cat(lc2015));
Map.addLayer(lc2015_4cat, {min:0, max:3, palette:['black','red','green','blue']}, 'lc2015_4cat me',0)
var lc2018_4cat = probToCat_4cat(changeTypology_4cat(lc2018));
Map.addLayer(lc2018_4cat, {min:0, max:3, palette:['black','red','green','blue']}, 'lc2018_4cat me',0)
var lc2021_4cat = probToCat_4cat(changeTypology_4cat(lc2021));
Map.addLayer(lc2021_4cat, {min:0, max:3, palette:['black','red','green','blue']}, 'lc2021_4cat me',0)

var lcChange1_me = getChangeImg_4cat(lc2015_4cat, lc2018_4cat);
Map.addLayer(lcChange1_me.randomVisualizer(), {}, 'lcChange1 me',0)
var lcChange2_me = getChangeImg_4cat(lc2018_4cat, lc2021_4cat);
Map.addLayer(lcChange2_me.randomVisualizer(), {}, 'lcChange2 me',0)
Map.addLayer(lcChange2_me.eq(3).selfMask(), {palette:['red']}, 'lcChange2 selected',0)
var lcChange3_me = getChangeImg_4cat(lc2015_4cat, lc2021_4cat);
Map.addLayer(lcChange3_me.randomVisualizer(), {}, 'lcChange3 me',0)
//Map.addLayer(lcChange3_me.eq(3).selfMask(), {palette:['red']}, 'lcChange3 selected',0)

Export.image.toDrive({
  image: lc2021Cat.clip(aoi),
  description: 'lc2021Cat_me',
  region: aoi,
  scale: 10,
  crs: 'EPSG:32632',
})
lcChange3_me = lcChange3_me.multiply(10).clip(aoi)
Export.image.toDrive({
  image: lcChange3_me,
  description: 'lcChange2015to2021Cat_me',
  region: aoi,
  scale: 10,
  crs: 'EPSG:32632',
})

//// stratified sample change 
getSampleSizeRecommendation(lcChange1_me, aoi, [
  0.8, 0.6, 0.6, 0.6, 
  0.6, 0.8, 0.6, 0.6, 
  0.6, 0.6, 0.8, 0.6, 
  0.6, 0.6, 0.6, 0.8], 0.02, 'sample_oslo_change_me_2015_2018')
  
getStratSample_change(lcChange1_me, probToCat_8cat(lc2015),probToCat_8cat(lc2018),   aoi, 
  [1,2,3,4,
    5,6,7,8,
    9,10,11,12,
    13,14,15,16], 
  [42, 25, 25, 25, 
  25, 56, 25, 25, 
  25, 25, 246, 25, 
  25, 25, 25, 37],
  'sample_oslo_change_me_2015_2018',  123, 234)


getSampleSizeRecommendation(lcChange2_me, aoi, [
  0.8, 0.6, 0.6, 0.6, 
  0.6, 0.8, 0.6, 0.6, 
  0.6, 0.6, 0.8, 0.6, 
  0.6, 0.6, 0.6, 0.8], 0.02, 'sample_oslo_change_me_2018_2021')

getStratSample_change(lcChange2_me, probToCat_8cat(lc2018),probToCat_8cat(lc2021), aoi, 
  [1,2,3,4,
    5,6,7,8,
    9,10,11,12,
    13,14,15,16], 
  [43, 25, 25, 25, 
  25, 58, 25, 25, 
  25, 25, 247, 25, 
  25, 25, 25, 37],
  'sample_oslo_change_me_2018_2021',  123, 234)
  
  

getSampleSizeRecommendation(lcChange3_me, aoi, [
  0.6, 0.6, 0.6, 0.6, 
  0.6, 0.6, 0.6, 0.6, 
  0.6, 0.6, 0.6, 0.6, 
  0.6, 0.6, 0.6, 0.6], 0.02, 'sample_oslo_change_me_2015_2021')

getStratSample_change(lcChange3_me,probToCat_8cat(lc2015),probToCat_8cat(lc2021),  aoi, 
  [1,2,3,4,
    5,6,7,8,
    9,10,11,12,
    13,14,15,16], 
  [62, 25, 25, 25, 
  25, 81, 25, 25, 
  25, 25, 356, 25, 
  25, 25, 25, 48],
  'sample_oslo_change_me_2015_2021',  123, 234)

/*
  // Cross-sampler comparison sample ///////////////////////////////////////////////////////////////////////////
*/


//// stratified sample for calibration excercise - where multiple samplers will classify the same points
/// to get an sampling accuracy estimate
//getSampleSizeRecommendation(lc2018Cat, aoi, [0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8], 0.02, 'sample_oslo_calibrate')

getStratSample_simple(lc2018Cat, aoi, [1,2,3,4,5,6,7,8], [40,10,10,20,10,80,10,20], 'sample_oslo_calibrate', 12, 23)



/*
  //Global functions //////////////////////////////////////////////////////////////////////////////////
*/
function getChangeImg_4cat(start, end){
  start = start.remap([0,1,2,3],[2,4,6,8])
  end = end.remap([0,1,2,3],[2,4,6,8])
  var catChange = ee.Image(start.subtract(end))
    .add(ee.Image(start).pow(ee.Image(end)))
    .remap([
      4,	14,	60,	250,
      18,	256,	4094,	65532,
      40,	1298,	46656,	1679614,
      70,	4100,	262146,	16777216], 
    [1,2,3,4,
     5,6,7,8,
     9,10,11,12,
     13,14,15,16]);
  return catChange
}

function changeTypology_4cat(img){
    var other = ee.Image(img.select('water').add(img.select('snow_and_ice'))).rename('other')
    var grey = ee.Image(img.select('built').add(img.select('bare'))).rename('grey');
    var tree = ee.Image(img.select('trees').add(img.select('shrub_and_scrub'))).rename('tree');
    var grass = ee.Image(img.select('grass').add(img.select('flooded_vegetation')).add(img.select('crops'))).rename('grass');
    return grey.addBands(tree).addBands(grass).addBands(other)
}

function probToCat_4cat(img){
  var imgMax = img.reduce(ee.Reducer.max());
  var imgCat = ee.Image(0)
    .where(img.select('grey').eq(imgMax), 1)
    .where(img.select('tree').eq(imgMax), 2)
    .where(img.select('grass').eq(imgMax), 3)
    .updateMask(img.select(0).gte(0))
  return imgCat
}
function probToCat_8cat(img){
  var imgMax = img.reduce(ee.Reducer.max());
  var imgCat = ee.Image(0)
    .where(img.select('built').eq(imgMax), 1)
    .where(img.select('crops').eq(imgMax), 2)
    .where(img.select('bare').eq(imgMax), 3)
    .where(img.select('grass').eq(imgMax), 4)
    .where(img.select('shrub_and_scrub').eq(imgMax), 5)
    .where(img.select('trees').eq(imgMax), 6)
    .where(img.select('flooded_vegetation').eq(imgMax), 7)
    .where(img.select('water').eq(imgMax), 8)
  imgCat = imgCat.selfMask()
  return imgCat
}


function getSampleSizeRecommendation(stratImage, aoi, expectedAccuracies, targetSE, label){
  
  var STRATALIST = [];
  var AREAWEIGHTS = []
  var NUMSTRATA = [];

  var strataAreas = ee.Image.pixelArea().addBands(stratImage).reduceRegion({
    reducer: ee.Reducer.sum().group(1), 
    geometry: aoi, 
    scale: 10, 
    maxPixels: 1e13
  })
  .evaluate(function(obj) {
    // obj is a list of groups.
    var groups = obj.groups
    var strataAreas = [];
    var totalArea = 0;
    for (var i=0; i<groups.length; i++) {
      STRATALIST.push(groups[i].group)
      strataAreas.push(Math.round(groups[i].sum))
      totalArea+=groups[i].sum
    }

    // Compute the proprtional area
    var check = 0;
    for(var j=0; j<strataAreas.length; j++) {
      AREAWEIGHTS[j] = strataAreas[j] / totalArea;
      check += AREAWEIGHTS[j]
    }
    
    NUMSTRATA = STRATALIST.length
    
    var numeratorList = []
    var tse = targetSE
    for (var i = 0; i < NUMSTRATA; i++){
      var weight = Number(AREAWEIGHTS[i])
      var usAc = Number(expectedAccuracies[i])
      var numerator = weight * Math.sqrt(usAc * (1 - usAc))
      numeratorList.push(numerator)
    }
    
    var numeratorFull = 0
        for (var i=0; i<numeratorList.length; i++){
          numeratorFull += numeratorList[i]
        }
  
    var sampleSize = Math.round(Math.pow((numeratorFull / tse), 2))
    
    var equalAllocation = Math.round(sampleSize / NUMSTRATA)
    
    var propAllocation = []
        for (var i=0; i<AREAWEIGHTS.length; i++){
           var propS = AREAWEIGHTS[i] * sampleSize
           propAllocation.push(Math.round(propS))
        }
    
    
    var dict = {
      'NUMSTRATA': NUMSTRATA,
      'STRATALIST': STRATALIST,
      'AREAWEIGHTS':AREAWEIGHTS,
      'Strata Areas [m2]:': strataAreas,
      'sampleSize':sampleSize,
      'equalAllocation':equalAllocation,
      'propAllocation':propAllocation
    }
    StrataAreas = StrataAreas.merge(ee.FeatureCollection(ee.Feature(null, {batch: label, areaList : strataAreas})))
    print(label, dict)
    
    if (label == 'sample_oslo_change_me_2015_2021'){
      Export.table.toDrive({
        collection: StrataAreas,
        description: 'strataAreas',
        fileFormat: 'CSV'
      })
    }
    
    
  })
  
}


function getStratSample_simple(stratImage, aoi, stratList, sampleSizeList, exportLabel, seed1, seed2){
  
  var sample = stratImage.rename('LC').stratifiedSample({
     seed: seed1,
     numPoints: 0, // 0 points for pixel values not in 'allocation'
     region: aoi,
     classBand: 'LC', // class band name
     classValues: stratList, // pixel values
     classPoints: sampleSizeList, // sample allocation
     tileScale: 2,
     projection: proj,
     scale: 10, 
     geometries: true
  })
  
  sample = sample.randomColumn('PLOTID',seed2)
  sample = sample.map(function(ft){
    var id = ee.String('id_').cat(ee.String(ee.Number(ft.get('PLOTID')).multiply(ee.Number(1e8)).round().int()))
    return ft
      .set(
        'PLOTID', id,
        'batch', exportLabel)
  })
  //print(sample.limit(10))
  //print(sample.size())
  //Map.addLayer(sample, {}, 'sample')
  
  Export.table.toAsset({
    collection: sample.sort('PLOTID').select(['PLOTID', 'LC', 'batch']), 
    assetId: 'GIS_synergy/Extent/Sampling/' + exportLabel ,
    description: exportLabel
  })
  
}

function getStratSample_change(stratImage, startImg, endImg, aoi, stratList, sampleSizeList, exportLabel, seed1, seed2){
  
  var sample = stratImage.rename('changeLC')
    .addBands(startImg.rename('startLC'))
    .addBands(endImg.rename('endLC')).stratifiedSample({
     seed: seed1,
     numPoints: 0, // 0 points for pixel values not in 'allocation'
     region: aoi,
     classBand: 'changeLC', // class band name
     classValues: stratList, // pixel values
     classPoints: sampleSizeList, // sample allocation
     tileScale: 2,
     projection: proj,
     scale: 10, 
     geometries: true
  })
  
  sample = sample.randomColumn('PLOTID',seed2)
  sample = sample.map(function(ft){
    var id = ee.String('id_').cat(ee.String(ee.Number(ft.get('PLOTID')).multiply(ee.Number(1e8)).round().int()))
    return ft
      .set(
        'PLOTID', id,
        'batch', exportLabel)
  })
  //print(sample.limit(10))
  //Map.addLayer(sample, {}, 'sample')
  
  Export.table.toAsset({
    collection: sample.sort('PLOTID').select(['PLOTID', 'changeLC', 'startLC','endLC', 'batch']), 
    assetId: 'GIS_synergy/Extent/Sampling/' + exportLabel ,
    description: exportLabel
  })
  
}
