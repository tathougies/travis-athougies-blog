#!python -u
from PIL import Image
import sys
import cStringIO as StringIO

_, maxSize = sys.argv

data = sys.stdin.read()
infile = StringIO.StringIO(data)

img = Image.open(infile)
oimg = img

maxSize = int(maxSize)
img.thumbnail((maxSize, maxSize))

exif = img._getexif() if hasattr(img, '_getexif') else None
if exif is None:
    orientation = 0
else:
    try:
        orientation = exif[0x0112]
    except KeyError, e:
        orientation = 0
if orientation == 3:
    img = img.rotate(180)
elif orientation == 6:
    img = img.rotate(270)
elif orientation == 8:
    img = img.rotate(90)

output = StringIO.StringIO()
img.save(output, oimg.format)
sys.stdout.write(output.getvalue())
