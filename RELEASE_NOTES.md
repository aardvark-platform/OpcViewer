### 1.9.1
- fixed verbosity in kdtree loading

### 1.9.0
- moved Lazy Kdtree Structures to old namespace to prevent deserialization troubles.

### 1.8.1
- ported over KdTree loading optimization from https://github.com/pro3d-space/PRo3D/pull/418

### 1.8.0
- Updated to Aardvark.Base 5.3
- Added Prinziple support
- Renamed Aardvark.VRVis.Opc namespace to OpcViewer.Base

### 1.7.2
- cleaned up kd loading code

### 1.7.1
- reactivated master kdtree path

### 1.7.0
- Optimized KdTree loading

### 1.6.1
- changed KdTree loading order in favor of lazy kdtree's and rebuilding fading out InCore Level0 KdTrees.

### 1.6.0 
- exposed kdtree build flags 

### 1.5.2 
- creates relative paths for kdtrees as early as possible

### 1.5.1
- robustness for readonly file systems, validation and repair of broken cache files if possible.

### 1.5.0
- added option to prevent new file construction for read only file systems & to control loading runtime


### 1.4.1
- LazyKdTree caches need to be list not array (accidently introduced serialization issue)


### 1.4.0
- KdTree loading now parametrized and allows to create kdtrees

### 1.3.0
- updated to Aardvark.Rendering 5.4
- [Rabbyte] simplified model and actions by using dropdownUnclearable

### 1.2.0
- updated to Aardvark.Rendering 5.3
- cleaned dependencies

### 1.1.1
- updated media to 5.3

### 1.1.0
- updated to Aardvark 5.2 libraries

### 1.0.11
- updated to f# 5

### 1.0.10
- updated packages

### 1.0.8
- updated packages

### 1.0.7
- vulkan shader compatibility

### 1.0.6
- upgraded to base 5.1/adaptive 1.1 track
