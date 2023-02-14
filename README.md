# 1-bit image codec

 - minimum image dimensions: 16x16
 - maximum image dimensions (in theory): 1048576x1048576 (roughly 1M pixels per side)
 - maximum output colours: 2

images will be cut down in size (width and height) to nearest multiple of 16 during encoding. They will be decoded into PNG format. 

