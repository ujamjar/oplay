# oplay

## YUV video player 

### Command line

```
oplay [options] [in_file]

  -s      input size
  -S      output size
  -f      YUV format
  -r      frame rate (FPS)
  -d      start in fullscreen mode
  -v      be verbose
  -help   Display this list of options
  --help  Display this list of options
```

Sizes are specified with a string like `640x480` or an identifer 
like `qcif`, `vga`, `720p`(see stdsizes.ml for all supported).

The format is one of `yuy2, `uyvy`, `yvyu`, `yv12`, `iyuv`, `420`.

### Keys

* `escape` quite
* `space`, `return` play/stop
* `tab` toggle full screen
* `right`, `left` step one frame forward/backward
* `home`, `end` go to first/last frame
* `1`-`9` go `n`*10% into the file

### TODO

* Planar 422 support
* File diff support
* Show individual colour planes
* Grid overlay

