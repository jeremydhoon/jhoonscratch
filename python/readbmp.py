import struct

def read_bmp(fname):
  infile = open(fname, 'rb')
  data = infile.read()
  infile.close()
  header_format = [
    ("Magic number", "cc"), #BM magic number
    ("File size", "i"), #size
    ("reserved 1", "H"),
    ("reserved 2", "H"),
    ("data offset", "i"),
    ("header size", "I"),
    ("width px", "I"),
    ("height px", "I"),
    ("color planes", "H"),
    ("px depth", "H"),
    ("compression method", "I"),
    ("image size", "I"),
    ("horiz res", "I"),
    ("vertical res", "I"),
    ("num colors", "I"),
    ("num important colors", "I")
  ]
  def dump_header(data):
    offset = 0
    for label, fmt in header_format:
      print label, struct.unpack_from(fmt,data,offset)
      offset += struct.calcsize(fmt)
    print "Final offset:", offset
  dump_header(data)
  
if __name__ == '__main__':
    read_bmp('./out.bmp')