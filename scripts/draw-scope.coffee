lineOfVariableUse = 4
usageCol = [22, 30]

console.clear()

_log = ->
  console.log.apply console, arguments
  arguments[0]
_logt = (args) -> 
  _log (_.map args, (arr) -> "(#{arr.join(',')})")
  args[0]

concat = (arr1, arr2) -> arr1.concat arr2
prepend = (val, arr) -> [val].concat arr
append = (val, arr) -> arr.concat [val]
slice = (arr, start, count) -> arr.slice start, count
splitOnIdx = (idx, arr) -> [(_.first arr, idx+1), (slice arr, idx+1)]
reverse = (arr) -> arr.reverse()
inc = (cnt,arr) -> _.map arr, (x) -> x+cnt
calcDerivatives = (vector) -> switch
  when vector.length <= 1 then []
  else prepend (vector[0] - vector[1]), (calcDerivatives _.tail vector)

code = d3.select '#code'
svg = code.append 'svg'

[lineCount, indentations] = do ->
    lines = code.select('pre').node()
              .innerHTML.split '\n'
    [
     lines.length, 
     lines.map (line) -> /^\s*/.exec(line)[0].length
    ]

pxPerLine = code.select('pre').node().scrollHeight / lineCount

xpos = (d) -> 10+ 7*d
ypos = (d, t) -> t*pxPerLine

lineColor = (c, node) ->
  node
    .attr 'stroke-width', 2
    .attr 'fill', 'none'
    .attr 'stroke', c

interpolateVerticalSquareWave = (points) -> switch
  when points.lenght == 0 then []
  when points.length == 1 then [points[0], [points[0][0], (points[0][1]+pxPerLine)]]
  else 
    intermediate = [points[0][0], points[1][1]]
    concat [points[0], intermediate], (interpolateVerticalSquareWave _.tail points)

line = d3.svg
          .line()
          .x xpos
          .y ypos
          .interpolate interpolateVerticalSquareWave

drawPaths = ->        
  path = lineColor 'brown', 
          svg
            .append 'path'
            .attr 'd', line indentations
  len = path.node().getTotalLength()
  #http://stackoverflow.com/questions/26813907/draw-a-path-out-gradually-from-the-middle/26814398
  path
    .attr 'stroke-dasharray', ('0 ' + len)
    .attr 'stroke-dashoffset', len/2
    .transition()
    .duration 1000
    .attr 'stroke-dasharray', (len + ' 0')
    .attr 'stroke-dashoffset', len
  
  
drawUsage = ->
  usageLine = d3.svg
               .line()
               .x -> -10 + xpos.apply null, arguments
               .y 2*pxPerLine + ypos null, lineOfVariableUse
  
  lineColor 'red', 
    svg
      .append 'path'
      .attr 'd', usageLine usageCol

walkPaths = ->        
  splitIndents = (arr) ->
    splitUp = splitOnIdx lineOfVariableUse, arr
    [(reverse splitUp[0]), splitUp[1]]
  indents = splitIndents indentations
  
  #([8, 8, 4, 8, 8, 4, 0]) -> [8, 8, 4, 4, 4, 4, 0]
  smoothOutDips = (rest) ->
    return rest if rest.length < 2
    current = rest[0]
    next = if rest[1] > current then current else rest[1]
    prepend current, (smoothOutDips (prepend next, (slice rest, 2) ) )
    
  before = smoothOutDips indents[0]
  after = smoothOutDips indents[1]
  smoothedIndents = concat (reverse before), after

  breaks = do ->
    derivatives = _log calcDerivatives smoothedIndents
    findIdx = (startAt, step, ifNotFound, test) ->
      endAt = if step < 0 then 0 else derivatives.length
      idx = _.find (_.range startAt, endAt, step), (i) -> test derivatives[i]
      if idx? then idx else ifNotFound
    getBreaks = (start, end) ->
      return [[start, end]] if start == 0 and end >= derivatives.length-1
      s = findIdx start-1, -1, 0, (x) -> x < 0
      e = findIdx end+1,  1, derivatives.length, (x) -> x > 0
      prepend [start, end], (getBreaks s, e)
    b = getBreaks lineOfVariableUse-1, 
            (findIdx lineOfVariableUse, 1, derivatives.length, (v) -> v > 0)
    append [0, indentations.length], (_.map b, (t) -> inc 1, t)
    
  clip = svg
    .append 'defs'
    .append 'clipPath'
    .attr 'id', 'visible-smoothed'
      .append 'rect'
      .attr 'x', 0
      .attr 'width', '100%'
      .attr 'y', 0
      .attr 'height', '100%'
  setClipPath = (step) -> 
    bk = breaks[step]
    clip
      .attr 'y', pxPerLine*bk[0] + 1
      .attr 'height', pxPerLine *(bk[1]- bk[0]) - 2
    
  step = 0
  setClipPath step
  d3.select('button.next-scope')
    .attr 'disabled', null
    .node().addEventListener 'click', ->
      setClipPath ( (step+=1) % breaks.length)

  line = lineColor 'lime', 
    svg
      .append 'g'
      .attr 'transform', 'translate(-2, 0)'
      .append 'path'
      .attr 'd', line smoothedIndents
      .attr 'stroke-width', 1
      .attr 'clip-path', 'url(#visible-smoothed)'
  line.attr 'stroke-width', 1
      
wait = (ms) -> -> 
  d = Q.defer()
  setTimeout d.resolve, ms
  d.promise

  
(do wait 1000)
  .then drawUsage
  .then wait 2000
  .then drawPaths
  .then wait 2000
  .then -> code.classed 'rotate', true
  .then wait 2000
  .then -> code.classed 'rotate', false
  .then wait 1000
  .then walkPaths
  .done()
  

  
  
  