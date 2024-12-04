# vox-postproc

Postprocessing after mariogeiger/obj2voxel. The aforementioned tool converts a
3D surface in a Wavefront OBJ format to voxels (as a Numpy array). The output
array is flat and its element type is `uint8`.

This tool does 2 things:

* Transforms the array to a multidimensional array of type `bool`.
* Fills interior of your model with voxels (i.e. it adds volume).

The output can be saved to numpy arrays or raw data (8 bits per voxel).
