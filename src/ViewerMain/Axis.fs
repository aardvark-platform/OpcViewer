namespace OpcSelectionViewer

open System
open System.IO
open Aardvark.Base
open OpcViewer.Base.Picking

module AxisFunctions = 
  
  type AxisPoint = {
    index           : int
    point           : OrientedPoint
    T               : double
    normalDev       : double
    planeHeight     : double
    planeHeightSign : int    
    }

  type Cursor<'a> =
    {
       before  : 'a
       current : 'a
       after   : 'a
    }

  module Cursor =      
    let map (f : 'a-> 'b) (a:Cursor<'a>) : Cursor<'b> =
      {
        before  = f a.before
        current = f a.current
        after   = f a.after
      }
 

  let createDomainAxis (ops : OrientedPoint[]) : OpcSelectionViewer.Axis = 
      let length = 
        ops
          |> List.ofArray 
          |> List.pairwise 
          |> List.fold (fun l (a,b) -> l + (b.position - a.position).Length) 0.0
        
      {
          positions        = ops |> Array.map (fun x -> x.position) |> List.ofArray
          pointList        = ops |> PList.ofArray
          selectionOnAxis  = None
          length           = length
          rangeSv          = if length > 1.0 then 
                               ops |> Array.map(fun x -> x.stationing.sv) |> Range1d
                             else Range1d.Invalid
      }
    
  let loadAxis (path : string) : Option<OpcSelectionViewer.Axis> = 
    if File.Exists path then
      let orientedPoints = OpcSelectionViewer.Serialization.loadAsType<OrientedPoint[]> path
      Some (createDomainAxis orientedPoints)
    else
      None
    
  let orientedPointtoAxisPoint (index : int) (queryPoint : V3d) (t : double) (op : OrientedPoint) = 
    let plane = new Plane3d(op.direction, op.position)
    let height = plane.Height(queryPoint)

    {
        index = index
        point = op
        T = t
        normalDev = 0.0
        planeHeight = height
        planeHeightSign = height.Sign()
    }

  let getTFromIndex (index : int) (axis : OpcSelectionViewer.Axis) = 
    double(index) / ((double)axis.positions.Length - 1.0)

  let axisPointFromIndex (queryPoint : V3d) (axis : OpcSelectionViewer.Axis) (index : int) = 
        let op = axis.pointList.Item index
        let plane = Plane3d (op.direction, op.position)
        let height = plane.Height (queryPoint)
        
        {
            index               = index
            point               = op
            T                   = getTFromIndex index axis
            normalDev           = Double.NaN
            planeHeight         = height
            planeHeightSign     = height.Sign()
        }

  let getNearestPointIndex (p : V3d) (axis : OpcSelectionViewer.Axis) = 
      let minPos = 
          axis.positions              
            |> List.minBy (fun x -> V3d.DistanceSquared(p, x))
  
      axis.positions             
          |> List.findIndex (fun x -> V3d.ApproxEqual(x, minPos))
  
  let getAxisPointOfOppositeSign(axisPoints : Cursor<AxisPoint>) : AxisPoint = 
    let signs = 
      axisPoints |> Cursor.map(fun x -> x.planeHeightSign)

    if(signs.before <> signs.current) then
      axisPoints.before
    else if (signs.after <> signs.current) then
      axisPoints.after
    else
      axisPoints.current    

  let getIndexFromFloatIndex (index : float) = 
    if index.IsNaN() then 0 else int (index)

  let getFloatingPointIndexFromT (axis : OpcSelectionViewer.Axis) (t : double) =
    let clampedT = clamp 0.0 1.0 t
    let pointcount = float (axis.pointList.Count-1)
    let index = (pointcount * clampedT)
    index

  let linearInterpolationofPoints (x : OrientedPoint) (y : OrientedPoint) (weight : float) = 
    {
      position = ((x.position * weight) + (y.position * (1.0 - weight)))
      direction = ((x.direction * weight) + (y.direction * (1.0 - weight)))
      stationing =
          {
              sh = ((x.stationing.sh * weight) + (y.stationing.sh * (1.0 - weight)))
              sv = ((x.stationing.sv * weight) + (y.stationing.sv * (1.0 - weight)))
          }
      offsetToMainAxisPoint = ((x.offsetToMainAxisPoint * weight) + (y.offsetToMainAxisPoint * (1.0 - weight)))
    }

  let getPositionFromT (axis : OpcSelectionViewer.Axis) (t : double) = 
    let floatIndex = t |> getFloatingPointIndexFromT axis
    let index = getIndexFromFloatIndex floatIndex

    let interpolant = 1.0 - (floatIndex - float(index))
   
    let selectedPoint = axis.pointList.Item index
    
    if (index >= axis.pointList.Count - 1) then 
        selectedPoint
    else 
        let nextPoint = (axis.pointList.Item (index + 1))
        linearInterpolationofPoints selectedPoint nextPoint interpolant
   
  let insertNewAxisPoint (axisPoints : Cursor<AxisPoint>) (newPoint : AxisPoint) =
    let signs = 
      axisPoints |> Cursor.map(fun x -> x.planeHeightSign)
    
    if(signs.before <> signs.current) then
      {
        before = axisPoints.before
        current = newPoint
        after = axisPoints.current
      }
    else if (signs.after <> signs.current) then
      {
        before = axisPoints.current
        current = newPoint
        after = axisPoints.after
      }
    else
      axisPoints    

  let rec findSoundingPointIteratively (depth : int) (axisPoints : Cursor<AxisPoint>) (queryPoint : V3d) (epsilon : double) (maxdepth : int) (axis : OpcSelectionViewer.Axis) =        
    let bound = getAxisPointOfOppositeSign (axisPoints)
    let interPolT = (axisPoints.current.T + bound.T) * 0.5
    let interPolPos = interPolT |> getPositionFromT axis
   
    let newAxisPoint = orientedPointtoAxisPoint -1 queryPoint interPolT interPolPos
   
    if (newAxisPoint.planeHeight.Abs() < epsilon) || (depth > maxdepth) 
        then newAxisPoint
    else 
        let cursor = insertNewAxisPoint axisPoints newAxisPoint
        findSoundingPointIteratively (depth+1) cursor queryPoint epsilon maxdepth axis

  let getNearestTOnAxis (pos : V3d) (epsilon : double)  (maxDepth : int) (axis : OpcSelectionViewer.Axis) = 
      let index = getNearestPointIndex pos axis        
 
      let axisPointCursor =
        {
          before  = Fun.Max (index - 1, 0)
          current = index
          after   = Fun.Min (index + 1, axis.positions.Length-1)
        } 
      |> Cursor.map (axisPointFromIndex pos axis)
         
      let closest = 
          findSoundingPointIteratively 0 axisPointCursor pos epsilon maxDepth axis
 
      closest.T
 
  let getNearestPointOnAxis' (pos : V3d) (axis : OpcSelectionViewer.Axis) = 
    let t = getNearestTOnAxis pos 1e-5 10 axis |> clamp 0.0 1.0      
    let p = t |> getPositionFromT axis
    (p,t)

  let calcDebuggingPosition (points : plist<V3d>) (axis : Option<OpcSelectionViewer.Axis>) =
    axis
      |> Option.map(fun a -> 
          let pointsOnAxis = 
             points
               |> PList.map(fun p -> getNearestPointOnAxis' p a)
               |> PList.toList

          match pointsOnAxis |> List.isEmpty with
          | true -> {a with selectionOnAxis = None}
          | false -> 
            let midPoint = 
              pointsOnAxis
              |> List.averageBy (fun (b,t) -> t) 
              |> fun x -> (getPositionFromT a x).position

            { a with selectionOnAxis = Some midPoint }
      )

  let pointsOnAxis (axis : Option<OpcSelectionViewer.Axis>) (points : plist<V3d>) =
    axis
      |> Option.bind(fun a -> 
        let pointsOnAxis = 
          points
            |> PList.map(fun p -> getNearestPointOnAxis' p a)
           
        match pointsOnAxis |> PList.isEmpty with
        | true -> None
        | false -> 
          let midPoint = 
            pointsOnAxis
             |> PList.toList
            |> List.averageBy (fun (b,t) -> t) 
            |> fun x -> (getPositionFromT a x).position

          let axisPoints = pointsOnAxis |> PList.map(fun (b,t) -> b.position)

          Some
            {
              pointsOnAxis = axisPoints
              midPoint    = midPoint
            })