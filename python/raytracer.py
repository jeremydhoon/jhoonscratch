import array
import math
import struct

def write_bmp(fname, colors):
  h = len(colors)
  if not h: return
  w = len(colors[0])
  for r in colors:
    assert len(r) == w
  #first calculate some useful values
  rowsize = 4*(((24*w) + 31)/32)
  filesize = 54 + rowsize*h
  rowpadding = (4 - (3*w % 4)) % 4
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
  default_values = {
    'Magic number': 'BM',
    'File size': [filesize],
    'reserved 1': [0],
    'reserved 2': [0],
    'data offset': [54],
    'header size': [40],
    'width px': [w],
    'height px': [h],
    'color planes': [1],
    'px depth': [24],
    'compression method': [0],
    'image size': [0],
    'horiz res': [2835],
    'vertical res': [2835],
    'num colors': [0],
    'num important colors': [0]
  }
  print "FILE SIZE:", filesize
  print "ROW SIZE:", rowsize
  print "ROW PADDING", rowpadding
  zero = chr(0)
  buf = array.array('c', [zero for i in range(filesize)])
  offset = 0
  for k,v in header_format:
    struct.pack_into('<' + v, buf, offset, *default_values[k])
    offset += struct.calcsize(v)
  for r in colors:
    for c in r:
      for k in range(3):
	buf[offset + k] = chr(int(255.0*c.channels[2 - k]))
      offset += 3
    for k in range(rowpadding):
      buf[offset] = chr(0)
      offset += 1
  outfile = open(fname, 'wb')
  buf.tofile(outfile)
  outfile.close()   

def calc_rel_intensity2(dist2):
  return min([1.0, 3.75/(dist2)])

class Vector3(object):
  __slots__ = ['arr']
  def __init__(self, x, y, z):
    self.arr = array.array('d', [x,y,z])
    object.__init__(self)
  def setx(self,x): self.arr[0] = x
  def sety(self,y): self.arr[1] = y
  def setz(self,z): self.arr[2] = z
  def getx(self): return self.arr[0]
  def gety(self): return self.arr[1]
  def getz(self): return self.arr[2]
  def dot(self,other):
    ret = 0.0
    for a,b in zip(self.arr, other.arr):
	ret += a*b
    return ret
  def cross(self, other):
    return Vector3(self.gety()*other.getz() - other.gety()*self.getz(),
	self.getx()*other.getz() - other.getx()*self.getz(),
	self.getx()*other.gety() - other.getx()*self.gety())
  def sum(self,other):
    return Vector3(*[a+b for a,b in zip(self.arr, other.arr)])
  def difference(self,other):
    return Vector3(*[a-b for a,b in zip(self.arr, other.arr)])
  def mag2(self):
    return reduce(lambda acc,x: acc + x*x, self.arr, 0.0)
  def magnitude(self):
    return math.sqrt(self.mag2())
  def norm(self):
    divisor = self.magnitude()
    return Vector3(*[ele/divisor for ele in self.arr])
  def scale(self, scalar):
    return Vector3(*[ele*scalar for ele in self.arr])
  def equal(self, other, tolerance = 0.0001):
    for a,b in zip(self.arr, other.arr):
      if abs(a-b) > tolerance:
	return False
    return True
  def __add__(self,other):
    return self.sum(other)
  def __sub__(self,other):
    return self.difference(other)
  def __iadd__(self,other):
    for a,b,ndx in zip(self.arr,other.arr,range(3)):
      self.arr[ndx] = a+b
  def __isub__(self,other):
    for a,b,ndx in zip(self.arr,other.arr,range(3)):
      self.arr[ndx] = a-b
  x = property(getx,setx)
  y = property(gety,sety)
  z = property(getz,setz)

class Color(object):
  def __init__(self, *args):
    if len(args) != 3:
      raise ValueError, "Only 3-color schemes are supported."
    if isinstance(args[0], int):
      self.channels = self._parseIntChannels(args)
    elif isinstance(args[0], float):
      self.channels = self._parseFloatChannels(args)
    else:
     raise TypeError, "Unrecognized channel type: %s" % str(type(args[0]))      
  def _parseIntChannels(self, args):
     ret = []
     for ele in args:
      if not isinstance(ele,int):
	raise TypeError, "Cannot mix integer and float channel values."
      else:
	ret.append(max([min([1.0, float(ele)/255.0]), 0.0]))
     return ret
  def _parseFloatChannels(self, args):
     for ele in args:
      if not isinstance(ele,float):
	  raise TypeError, "Cannot mix integer and float channel values."
     constrain = lambda n: max([0.0, min([1.0,n])])
     return [constrain(n) for n in args]
  def scale(self, scalar):
    return Color(*[scalar*ele for ele in self.channels])
  def as_int_bigendian(self):
    pass
  def as_int(self):
    pass

class Shape(object):
  def __init__(self, v3, **kwargs):
    if not isinstance(v3, Vector3):
      raise TypeError, "Positional argument to Shape object must be a Vector3."
    self.pos = v3
    self.color = kwargs.pop('color')
  def intersect(self, source, dir):
      raise NotImplementedError, "Shape base class intersect method must be overridden."
  def normal(self, isect):
      raise NotImplementedError, "Shape base class normal method must be overridden."
    
class Light(object):
  def __init__(self, position, **kwargs):
    self.intensity = kwargs.pop('intensity', 1.0)
    self.position = position
    
class Camera(object):
  def __init__(self, pos, front, up, **kwargs):
    if not isinstance(front, Vector3):
      raise TypeError, "Argument 'front' must be a Vector3."
    if not isinstance(up, Vector3):
      raise TypeError, "Argument 'up' must be a Vector3."
    self.front = front.norm()
    self.up = front.cross(up).cross(front).norm() #make sure it's pendicular
    self.pos = pos #could check if it's a vector3
    self.viewing_angle_x = kwargs.pop('viewing_angle_x', math.pi/3.0)
  def get_rays(self, w, h):
    w = float(w)
    h = float(h)
    right = self.front.cross(self.up)
    viewing_angle_y = h*self.viewing_angle_x/w
    ret = []
    xincr = self.viewing_angle_x/w
    yincr = viewing_angle_y/h
    for xndx in range(int(-w/2), int(w/2)):
      for yndx in range(int(-h/2), int(h/2)):
	ret.append((
	    right.scale(math.sin(xincr*xndx*self.viewing_angle_x))
	  + self.up.scale(math.sin(yincr*yndx*viewing_angle_y)) \
	  + self.front).norm())
    return ret
  def set_front(self, fr):
     self.front = fr.norm()    

class Scene(object):
    def __init__(self, **kwargs):
	self.shapes = []
	self.lights = []
	self.camera = kwargs.pop('camera')
	self.bg_color = kwargs.pop('bg_color')
	self.ambient_lighting = 0.1
    def render(self, w, h):
      out = []
      rays = self.camera.get_rays(w,h)
      source = self.camera.pos
      tohandle = []
      count = 0
      for r in rays:
	intersections = []
	if count % w == 0:
	  out.append([])
	count += 1
	for s in self.shapes:
	  isect = s.intersect(source, r)
	  if isect is not None:
	    dist2 = (isect - source).dot(r)
	    if dist2 > 0:
	      intersections.append((dist2, isect, s))
	if len(intersections) > 0:
	  intersections.sort()
	  out[-1].append(self.get_color(intersections[0], source))
	else:
	  out[-1].append(None)
      for r in out:
	for c,cndx in zip(r, range(len(r))):
	  if c is None:
	    r[cndx] = self.bg_color
      return out
    def get_color(self, isect_eles, ray_source):
      dist, v3, shape = isect_eles
      ret = 0.0
      for l in self.lights:
	source = l.position
	direction = v3 - source
	ndir = direction.norm()
	light_dist2 = direction.mag2()
	found = False
	for s in self.shapes:
	  isect = s.intersect(source, ndir)
	  if isect is not None:
	    dist2 = (l.position - isect).mag2()
	    if dist2 - light_dist2 < -0.1:# and not v3.equal(isect):
	      found = True
	      break
	      #pass
	if not found:
	  shade = (v3 - source).norm().dot(shape.normal(v3).norm())
	  if shade < 0:	
	    shade = 0
	  ret += shade*(1.0-self.ambient_lighting) + self.ambient_lighting
	else:
	  ret += self.ambient_lighting
      return shape.color.scale(ret)
      
class Sphere(Shape):
  def __init__(self, v3, radius, **kwargs):
    self.radius = radius
    Shape.__init__(self, v3, **kwargs)
  def intersect(self, source, direction):
    square = lambda a: a*a
    assert abs(direction.mag2() - 1.0) < 0.001
    v = direction
    cntr = self.pos - source
    b = v.dot(cntr)
    discrim = square(b) - cntr.mag2() + self.radius
    if discrim < 0:
      return None
    sqdiscrim = math.sqrt(discrim)
    d1,d2 = b - discrim, b + discrim
    if d1 < d2 and d1 > 0:
      vscale = d1
    elif d2 < d1 and d2 > 0:
      vscale = d2
    else:
      return None
    return v.scale(vscale) + source
  def normal(self, isect):
    return (self.pos - isect).norm()

class Plane(Shape):
  def __init__(self, v3, norm, **kwargs):
    if not isinstance(norm, Vector3):
      raise TypeError, "Norm argument must be a Vector3 instance."
    self.norm = norm.norm()
    Shape.__init__(self, v3, **kwargs)
    self.d = self.pos.dot(self.norm)
  def intersect(self, source, direction):
    ndir = direction.norm()
    denom = ndir.dot(self.norm)
    if abs(denom) < 0.0001:
      return None
    t = (self.d - source.dot(self.norm)) / denom
    if t <= 0.0001:
      return None
    return ndir.scale(t) + source
  def normal(self, isect):
    return self.norm.norm().scale(1.0)

if __name__ == '__main__':
    origin = Vector3(0.0,0.0,0.0)
    e0 = Vector3(1.0, 0.0, 0.0)
    e1 = Vector3(0.0, 1.0, 0.0)
    e2 = Vector3(0.0, 0.0, 1.0)
    
    red = Color(1.0, 0.0, 0.0)
    green = Color(0.0, 1.0, 0.0)
    blue = Color(0.0, 0.0, 1.0)
    grey = Color(.25, .25, .25)
    white = Color(1.0, 1.0, 1.0)
    
    cam = Camera(origin, e0, e1)
    light1 = Light(e0.scale(5.0) + e2.scale(-9.0), intensity=1.0)
    sph = Sphere(e0.scale(5.0), 3.0, color=red)
    pl = Plane(e0.scale(9.0), e0.scale(1.0), color=white)
    pl2 = Plane(e2.scale(3.0), e2, color=green)
    sc = Scene(camera=cam, bg_color=green)
    sc.lights.append(light1)
    sc.shapes.append(sph)
    sc.shapes.append(pl)
    sc.shapes.append(pl2)
    write_bmp("./out_python.bmp", sc.render(512,512))
  