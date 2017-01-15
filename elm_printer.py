from collections import namedtuple
from interop import data_to_tri_mesh
import json

class Vector(namedtuple('Vector', ['x', 'y', 'z'])):
    __slots__ = ()
    def __str__(self):
        return 'Vector %s %s %s' % (self.x, self.y, self.z)
    def __repr__(self):
        return __str__(self)

class Record(dict):
    def __str__(self):
        return print_elm_record(self)
    def __repr__(self):
        return self.__str__()

class Literal(object):
    def __init__(self, data):
        self.data = data
    def __str__(self):
        return str(self.data)
    def __repr__(self):
        return __str__(self)

class Object(object):
    def __init__(self, obj):
        self.obj = obj

    def __str__(self):
        obj = self.obj
        if obj.data is None:
            name = obj.dupli_group.objects[0].data.name
        else:
            name = obj.data.name

        w,x,y,z = obj.rotation_quaternion
        quaternion = {'vector': Vector(x,y,z), 'scalar': w}
        frame = { 'position': Vector(*list(obj.location))
                , 'orientation': Record(quaternion) }
        record = { 'drawable': Literal('Just %s' % name)
                 , 'frame': Record(frame)
                 , 'scale': Vector(*list(obj.scale))
                 , 'effects': Literal('[]')
                 , 'velocity': Literal('Nothing')
                 , 'material': Literal('Color (vec4 1 0.2 0.5 1)')}
        return str(Record(record))

    def __repr__(self):
        return self.__str__()


def print_objects(objects, initScene='initScene'):
    """Exports init function and RawMesh definitions necessary to render
    objects.
    """
    # get the data meshes
    meshes = []
    for obj in objects:
        if obj.data is None:
            data = obj.dupli_group.objects[0].data
        else:
            data = obj.data

        if data not in meshes:
            meshes += [data]

    assignments = []
    for data in meshes:
        var = 'init' + data.name
        mesh = data_to_tri_mesh(data)

        assignments += [print_elm_assignment(var, print_elm_RawMesh_JSON(mesh))]

    assignments = '\n\n'.join(assignments)

    # set up the initScene function
    data = str([Object(obj) for obj in objects])
    init = print_elm_assignment(initScene, data)
    init = '%s: List Object\n%s' % (initScene, init)

    return init, assignments


def print_elm_record(d):
    text = []
    for k,v in d.items():
        if isinstance(v, str):
            text += [k + ': "%s"' % v]
        else:
            text += [k + ': ' + str(v)]

    text = '\n, '.join(text)
    text = '{ %s }' % text
    return text


def print_elm_assignment(var, data):
    var = var[0].lower() + var[1:]
    return '%s = \n  %s' % (var, data)


def print_elm_RawMesh_JSON(mesh):
    """
    """
    fields = 'p1','p2' ,'p3' ,'q1' ,'q2' ,'q3' ,'r1' ,'r2','r3'
    s = []
    for face in mesh:
        values = [p for v in face for p in v ]
        assert(len(values) == len(fields))
        triangle = json.dumps({f: v for f,v in zip(fields, values)})
        s += [triangle]
    s = '[' + '\n,'.join(s) + ']'
    return s


def print_elm_RawMesh(mesh):
    """Prints in Elm
    List (Vec3, Vec3, Vec3)
    """
    s = []
    for face in mesh:
        s += ['(%s)' % ','.join('vec3 %f %f %f' % v for v in face)]
    s = '[' + '\n,'.join(s) + ']'
    return s
