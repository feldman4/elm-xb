from importlib import reload
import rpyc
import remote
import pyperclip
import json

# map generation
import numpy as np
from scipy.spatial import Voronoi, Delaunay, ConvexHull
import networkx as nx
import sys
import noise

# do this in blender console
# import remote
# x = remote.start_server(locals())

# blender startup
c = remote.start_client()
h = c.root.exposed_get_head()

bpy = c.root.exposed_getmodule('bpy')
mathutils = c.root.exposed_getmodule('mathutils')
remote = c.root.exposed_getmodule('remote')
mesh = c.root.exposed_get_operator('bpy.ops.mesh')

Vector = mathutils.Vector
rlist = remote.local(list)
rmap = remote.local(map)


def obj_to_tri_mesh(obj):
    if isinstance(obj, str):
        obj = bpy.data.objects[obj]

    if obj.data is None:
        group = obj.dupli_group
        # one object in group
        obj = group.objects[0]

    return data_to_tri_mesh(obj.data)


def data_to_tri_mesh(data):
    mesh = []
    for face in data.polygons:
        tri = [data.vertices[idx].co.to_tuple() for idx in face.vertices]
        # triangulate in blender
        assert(len(tri) == 3)
        mesh += [tri]

    return mesh




def points_to_graph(points):
    dt = Delaunay(points)
    v = Voronoi(points)
    G = nx.Graph()


    for i, point in enumerate(points):
        face = [v.vertices[j]
                for j in v.regions[v.point_region[i]]
                if j != -1]
        G.add_node(i, x=point[0], y=point[1], face=face)

    indices, indptr = dt.vertex_neighbor_vertices
    for k,_ in enumerate(points):
        for j in indptr[indices[k]:indices[k+1]]:
            G.add_edge(k, j)

    return G


def simplify(points, n=2):
    """ Don't move if any corners are outside convex hull.
    """
    xmin, ymin = points.min(axis=0)
    xmax, ymax = points.max(axis=0)

    for _ in range(n):
        v = Voronoi(points)
        dt = Delaunay(points)
        new_points = []

        for ix, pt in zip(v.point_region, points):
            region = v.regions[ix]
            internal = lambda p: dt.find_simplex(p)>0
            if -1 not in region and all(map(internal, v.vertices[region])):
                # interior region
                pt = v.vertices[region].mean(axis=0)
            new_points += [pt]

            assert(pt.min() > 0)
        points = np.array(new_points)
        points -= points.min(axis=0)
        points[:,0] = points[:,0] * (xmax - xmin) + xmin
        points[:,1] = points[:,1] * (ymax - ymin) + ymin
    return np.array(points)

def find_segments(points):
    from numpy.linalg import norm
    v = Voronoi(points)
    dt = Delaunay(points)
    center = points.mean(axis=0)
    segments = []
    for (a,b), (c,d) in v.ridge_dict.items():
        d = v.vertices[d]
        if c==-1:
            c = v.points[[a, b]].mean(axis=0)
            if norm(c-center) > norm(d-center):
                c = d + ((c-d)/norm(c-d))
            else:
                c = d - ((c-d)/norm(c-d))
        else:
            c = v.vertices[c]
        segments += [(c,d)]
    return segments

def plot_map(graph, ax=None, border=(20,30)):

    points = np.array([[d['x'], d['y']] for i,d in graph.nodes(data=True)])
    segments = find_segments(points) # for plotting

    if ax is None:
        fig, ax = plt.subplots(figsize=(6,6))

    ax.scatter(points[:,0], points[:,1], c='red', s=10)
    for (x0, y0), (x1, y1) in segments:
        ax.plot([x0, x1], [y0, y1], 'gray')

    pmax, pmin = points.max(axis=0), points.min(axis=0)
    pdiff = pmax - pmin
    prange = np.vstack([pmin - pdiff*0.1, pmax+pdiff*0.1])
    ax.set_xlim(prange[:,0])
    ax.set_ylim(prange[:,1])

    for i in border:
        border = points[graph.neighbors(i)]
        ax.scatter(border[:,0], border[:,1], c='m', s=20)
        ax.scatter(points[i,0], points[i,1], c='b', s=20);

    for n,d in graph.nodes(data=True):
        if 'color' in d and 'face' in d:
            ax.add_patch(ptch.Polygon(d['face'], color=d['color'], zorder=-10))

    return ax


def blenderize(points, name='python_object', mesh_name='python_mesh'):
    from remote import local
    points = np.array(points)
    dt = Delaunay(points[:,:2])
    vertices = rlist(rmap(Vector, local(points.tolist())))
    edges, faces = [], []
    for simplex in dt.simplices:
        faces += [list(simplex)]
    faces = remote.local(faces)

    new_mesh = bpy.data.meshes.new(mesh_name)
    new_mesh.from_pydata(vertices, edges, faces)
    new_mesh.update()

    new_object = bpy.data.objects.new(name, new_mesh)
    bpy.context.scene.objects.link(new_object)

    return new_object


def find_border(points, bmin=0, bmax=1):
    points = np.array(points)
    v = Voronoi(points[:,:2])
    border = []
    for i, ir in enumerate(v.point_region):
        region = v.regions[ir]
        if -1 in region:
            border += [i]
            continue
        vertices = np.array([v.vertices[r]
                             for r in region if r != -1])
        if vertices.max() > bmax or vertices.min() < bmin:
            border += [i]
    return border

def get_points(graph):
    points = []
    for n,d in G.nodes(data=True):
        x, y = d['x'], d['y']
        if 'z' in d:
            points += [[x, y, d['z']]]
        else:
            points += [[x, y]]
    return np.array(points)

def add_height(graph, **kwargs):
    for n, d in G.nodes(data=True):
        x, y = d['x'], d['y']
        d.update({'z': noise.pnoise2(x, y, **kwargs)})
    return graph

def color_water(graph, sea_level=0.2):
    points = get_points(graph)
    sea_level = np.percentile(points[:,2], sea_level*100.)

    border = find_border(points)
    for n,d in G.nodes(data=True):
        color = 'beige'
        if n in border or d['z'] < sea_level:
            color = 'lightblue'
        d.update({'color': color})
    return graph
