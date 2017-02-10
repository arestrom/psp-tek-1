########################### part 1 ###########################
# https://medium.com/@mbostock/command-line-cartography-part-1-897aa8f8ca2c#.dprnmexnr

# a few parameters:
  # A metric (e.g., population density)
  # A geographic entity (e.g., census tract)
  # A source (e.g., ACS 2014 5-year estimate)

# But forget that, and just browse the 2014 cartographic boundary files here: http://www2.census.gov/geo/tiger/GENZ2014/shp/

# Given a state’s FIPS code (06 for California), you can now use curl to download the corresponding census tract polygons:
curl 'http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_06_tract_500k.zip' -o cb_2014_06_tract_500k.zip

# Next, unzip the archive to extract the shapefile (.shp), and some other junk:
unzip -o cb_2014_06_tract_500k.zip

# A quick way to check what’s in a shapefile is to visit mapshaper.org and drag the shapefile into your browser.

# instructions to install npm via brew in MacOSX: https://changelog.com/posts/install-node-js-with-homebrew-on-os-x

# instructions to install npm in Windows: http://blog.teamtreehouse.com/install-node-js-npm-windows

# it’s possible to view shapefiles directly in your browser. But binary shapefiles can be difficult to work with, so we’ll convert to GeoJSON: a web-friendly, human-readable format. My shapefile parser has a command-line interface, shp2json, for this purpose.
npm install -g shapefile

# Now use shp2json to convert to GeoJSON:
shp2json cb_2014_06_tract_500k.shp -o ca.json
shp2json Henderson_Watershed_Project.shp -o HWP.json
shp2json Henderson_Flowline.shp -o HF.json
topojson -o HWP-np.json Henderson_Watershed_Project.shp
shp2json WBD_PRJ_HYDRO.shp -o hydro.json

# Note that this also reads cb_2014_06_tract_500k.dbf, a dBASE file, defining feature properties on the resulting GeoJSON

# We could now display this in a browser using D3, but first we should apply a geographic projection. By avoiding expensive trigonometric operations at runtime, the resulting GeoJSON renders much faster, especially on mobile devices. Pre-projecting also improves the efficacy of simplification, which we’ll cover in part 3. To install d3-geo-projection’s command-line interface:
npm install -g d3-geo-projection

# Now use geoproject:
geoproject 'd3.geoConicEqualArea().parallels([34, 40.5]).rotate([120, 0]).fitSize([960, 960], d)' < ca.json > ca-albers.json

geoproject 'd3.geoConicConformal().parallels([47 + 30 / 60, 48 + 44 / 60]).rotate([120 + 50 / 60, -47]).fitSize([960, 960], d)' < HWP.json > HWP-conic.json

geoproject 'd3.geoConicEqualArea().parallels([47 + 30 / 60, 48 + 44 / 60]).rotate([120 + 50 / 60, -47]).fitSize([960, 960], d)' < HWP.json > HWP-eq.json 

# This d3.geoConicEqualArea projection is California Albers, and as its name suggests, is appropriate for showing California. It’s also equal-area, which is strongly recommended for choropleth maps as the projection will not distort the data. If you’re not sure what projection to use (https://github.com/d3/d3-geo-projection/blob/master/README.md), try d3-stateplane or search spatialreference.org.

# The projection you specify to geoproject is an arbitrary JavaScript expression. That means that you can use projection.fitSize (https://github.com/d3/d3-geo/blob/master/README.md#projection_fitSize) to fit the input geometry (represented by d) to the desired 960×960 bounding box!
# To preview the projected geometry, use d3-geo-projection’s geo2svg (https://github.com/d3/d3-geo-projection/blob/master/README.md#geo2svg):
geo2svg -w 960 -h 960 < ca-albers.json > ca-albers.svg
geo2svg -w 960 -h 960 < HWP-conic.json > HWP.svg
geo2svg -w 960 -h 960 < HWP-eq.json > HWP-eq.svg
geo2svg -w 960 -h 960 < HWP.json > HWP.svg

# If you followed along on the command line, you hopefully learned how to download and prepare geometry from the U.S. Census Bureau.

########################### part 2 ###########################
# https://medium.com/@mbostock/command-line-cartography-part-2-c3a82c5c0f3#.abo7cjfft

# Often the data we find online isn’t precisely what we want. Perhaps we need to join multiple data sources, or to convert formats (say from fixed-width text to CSV), or even just to drop unneeded fields to produce smaller files that are faster to download...

# To leverage the power of the command line, we simply need to convert our data into a format that fits a UNIX convention: lines of text. And since JSON is already text, we just need each line of text be a valid JSON value.

# Enter newline-delimited JSON (NDJSON), which is simply JSON values separated by newlines (\n). NDJSON combines the best of both worlds: the convenience of the command line for working with files, and the power of JavaScript and its myriad open-source modules. My ndjson-cli module provides tools for converting JSON to NDJSON, for manipulating NDJSON streams (filtering, mapping, joining) and more. To install:
npm install -g ndjson-cli

# To convert a GeoJSON feature collection to a newline-delimited stream of GeoJSON features, use ndjson-split:
ndjson-split 'd.features' \
   < ca-albers.json \
   > ca-albers.ndjson

ndjson-split 'd.features' \
   < HWP.json \
   > HWP.ndjson

ndjson-split 'd.features' \
   < HF.json \
   > HF.ndjson

# If you haven’t seen redirection before, the < operator tells a command (ndjson-split) to read from the specified file, while the > operator tells it to write to the specified file. There’s also the | operator, which lets you pipe the output of one command as input to another command, obviating the need for an intermediate file. 

# The output here looks underwhelmingly similar to the ca-albers.json we saw previously; the only difference is that there is one feature (one census tract) per line. But this is huge—it means we can now manipulate individual features! For example, to set each feature’s id using ndjson-map:
ndjson-map 'd.id = d.properties.GEOID.slice(2), d' \
   < ca-albers.ndjson \
   > ca-albers-id.ndjson

ndjson-map 'd.id = d.properties.Project_Nu.slice(2), d' \
   < HWP.ndjson \
   > HWP-id.ndjson

# This id will be needed to join the geometry with the population estimates, which we will now download from the Census Bureau’s API using curl:
curl 'http://api.census.gov/data/2014/acs5?get=B01003_001E&for=tract:*&in=state:06' -o cb_2014_06_tract_B01003.json

# The B01003_001E in the URL specifies the total population estimate, while the for and in values specify that we want data for each census tract in California. See the API documentation (http://www.census.gov/data/developers/data-sets/acs-5year.html) for details.

# The resulting file is a JSON array. To convert it to an NDJSON stream, use ndjson-cat (to remove the newlines), ndjson-split (to separate the array into multiple lines) and ndjson-map (to reformat each line as an object). You can run these individually, but here’s how to do it all in one go:
ndjson-cat cb_2014_06_tract_B01003.json \
>   | ndjson-split 'd.slice(1)' \
>   | ndjson-map '{id: d[2] + d[3], B01003: +d[0]}' \
>   > cb_2014_06_tract_B01003.ndjson

# Now, magic! Join the population data to the geometry using ndjson-join:
ndjson-join 'd.id' \
>   ca-albers-id.ndjson \
>   cb_2014_06_tract_B01003.ndjson \
>   > ca-albers-join.ndjson

# It may be hard to see in the screenshot, but each line in the resulting NDJSON stream is a two-element array. The first element (d[0]) is from ca-albers-id.ndjson: a GeoJSON Feature representing a census tract polygon. The second element (d[1]) is from cb_2014_06_tract_B01003.ndjson: an object representing the population estimate for the same census tract.

# To compute the population density using ndjson-map, and to remove the additional properties we no longer need:
ndjson-map 'd[0].properties = {density: Math.floor(d[1].B01003 / d[0].properties.ALAND * 2589975.2356)}, d[0]' \
>   < ca-albers-join.ndjson \
>   > ca-albers-density.ndjson

# The population density is computed as the population estimate B01003 divided by the land area ALAND. The constant 2589975.2356 = 1609.34² converts the land area from square meters to square miles.

# To convert back to GeoJSON, use ndjson-reduce and ndjson-map:
ndjson-reduce \
>   < ca-albers-density.ndjson \
>   | ndjson-map '{type: "FeatureCollection", features: d}' \
>   > ca-albers-density.json

# Or, using ndjson-reduce alone:
ndjson-reduce 'p.features.push(d), p' '{type: "FeatureCollection", features: []}' \
  < ca-albers-density.ndjson \
  > ca-albers-density.json

# we can use d3-geo-projection to quickly generate an SVG choropleth from the command line! To do that, first install D3:
npm install -g d3

# Next use ndjson-map, requiring D3 via -r d3, and defining a fill property using a sequential scale with the Viridis color scheme:
ndjson-map -r d3 \
>   '(d.properties.fill = d3.scaleSequential(d3.interpolateViridis).domain([0, 4000])(d.properties.density), d)' \
>   < ca-albers-density.ndjson \
>   > ca-albers-color.ndjson

# To convert the newline-delimited GeoJSON to SVG using geo2svg:
geo2svg -n --stroke none -p 1 -w 960 -h 960 \
>   < ca-albers-color.ndjson \
>   > ca-albers-color.svg

# This is not yet a good choropleth: as population density is not uniformly distributed, the color encoding should be transformed to better show the data, say by explicit thresholds or a power scale. A choropleth should have a title, a key, and contextual cues to help identify geography, such as county borders.

# you hopefully learned how to download data from the U.S. Census Bureau, join it to geometry, compute derived data, and preview it.

########################### part 3 ###########################
# https://medium.com/@mbostock/command-line-cartography-part-3-1158e4c55a1e#.mjx45gi6h

# The GeoJSON feature collection of census tracts we constructed previously was 13.6M. That’s fine for local viewing, but a bit large for the web! Fortunately there are ways to shrink geometry without apparent loss of detail. We can:
  # Simplify (e.g., remove coordinates per Visvalingham).
  # Quantize (e.g., remove digits, say 224.3021507494117 to 224.3).
  # Compress (e.g., remove redundant geometry).

# These are possible with GeoJSON, but we can do even better if we switch to a JSON dialect designed for efficient transport: TopoJSON. TopoJSON files are often 80% smaller than GeoJSON files, even without simplification

# TopoJSON represents lines and polygons as sequences of arcs rather than sequences of coordinates. Contiguous polygons (census tracts, counties, states, etc.) have shared borders whose coordinates are duplicated in GeoJSON. With hierarchical geometry, such as counties that compose into states, there’s even more duplication! By representing lines and polygons as sequences of arcs, repeating an arc does not require repeating coordinates. (For more, see How to Infer Topology. (https://bost.ocks.org/mike/topology/) )

# TopoJSON can be quantized, where coordinates are represented as small integers instead of floating-point values with many decimal places.

# For example, a sequence of points:
  # [545.7796789342211, 348.96136952241613]
  # [545.9825061954095, 349.29419494812123]
  # [546.3281879653109, 349.53210438248560]
  # [546.3147336879572, 348.77969898749300]
  # [546.5844757927035, 348.76960903081610]
  # [546.5889751031176, 348.76842131978400]
# Is first converted to integers by scaling, translating and rounding:
  # [  0, 403]
  # [231, 741]
  # [625, 982]
  # [610, 219]
  # [917, 208]
  # [922, 207]
# And then delta-encoded such that each successive x- and y-value is relative to the previous one:
  # [  0, 403]
  # [231, 338]
  # [394, 241]
  # [-15,-763]
  # [307, -11]
  # [ 5,   -1]
# Quantization does lose information, but typically a small-scale map does not require the full precision of the original geometry. 

# Best of all, TopoJSON facilitates topology-preserving simplification: we can simplify geometry without detaching shared borders. To get started, install the TopoJSON CLI:
npm install -g topojson

# Use geo2topo to convert to TopoJSON, reducing its size to 8.1M:
geo2topo -n \
>   tracts=ca-albers-density.ndjson \
>   > ca-tracts-topo.json

geo2topo -n \
  bounds=HWP.ndjson \
  water=HF.ndjson \
  > b_w.json

  topo2geo bounds=b.json < b_w.json

geo2topo -n \
  bounds=HWP.ndjson \
  > bounds.json

# The slightly peculiar syntax, tracts=…, allows you to specify multiple named GeoJSON inputs, resulting in a topology with multiple named objects (or “layers”). Arcs can be shared across all objects in a topology.

# Now to toposimplify, further reducing to 3.1M:
toposimplify -p 1 -f \
>   < ca-tracts-topo.json \
>   > ca-simple-topo.json

# The -p 1 argument tells toposimplify to use a planar area threshold of one square pixel when implementing Visvalingham’s method; this is appropriate because we previously applied a conic equal-area projection. If simplifying before projecting, use -s and specify a minimum-area threshold in steradians instead. The -f says to remove small, detached rings—little islands, but not contiguous tracts—further reducing the output size.

# Lastly to topoquantize and delta-encode, reducing to 1.6M:
topoquantize 1e5 \
>   < ca-simple-topo.json \
>   > ca-quantized-topo.json

# Now suppose we want to overlay county borders on our choropleth map of census tracts. Most readers probably aren’t familiar with the geography of census tracts, so county outlines provide a helpful cue. (If we were making a national choropleth, we might similarly want state borders.)

# The Census Bureau also publishes county boundaries, but we don’t actually need them. TopoJSON has another powerful trick up its sleeve: since census tracts compose hierarchically into counties, we can derive county geometry using topomerge!
topomerge -k 'd.id.slice(0, 3)' counties=tracts \
  < ca-quantized-topo.json \
  > ca-merge-topo.json

# The -k argument defines a key expression that topomerge will evaluate to group features from the tracts object before merging. (It’s similar to nest.key in d3-collection.) The first three digits of the census tract id represent the state-specific part of the county FIPS code, so the census tracts for each county will be merged, resulting in county polygons. The result forms a new counties object on the output topology.

# Now, we don’t actually want the full county polygons; we want only the internal borders—the ones separating counties. (Stroking exterior borders tends to lose detail along coastlines.) We can also compute these with topomerge. A filter (-f) expression is evaluated for each arc, given the arc’s adjacent polygons a and b. By convention, a and b are the same on exterior arcs, and thus we can overwrite the counties object with a mesh of the internal borders like so:
topomerge --mesh -f 'a !== b' counties=counties \
  < ca-merge-topo.json \
  > ca-topo.json

# If you followed along on the command line, you hopefully learned how to convert GeoJSON to TopoJSON, to simplify and quantize topologies, and to merge features.