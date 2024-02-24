using System;
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;

public class MeshGenerator {
    public float perlinScale = 10f;
    public AnimationCurve terrainBumpiness;

    public float radius;
    public float height;

    public float pointDensity;
    public int outerPoints;
    public MeshFilter meshFilter;
    public MeshCollider meshCollider;

    private Mesh mesh;

    private int vertCount;
    List<Vector3> verticies;
    List<int> triangles;

    public void Initialise(
        float perlinScale,
        AnimationCurve terrainBumpiness,
        float radius,
        float height,
        float pointDensity,
        int outerPoints,
        MeshFilter meshFilter,
        MeshCollider meshCollider
    ) {
        this.perlinScale = perlinScale;
        this.terrainBumpiness = terrainBumpiness;

        this.radius = radius;
        this.height = height;

        this.pointDensity = pointDensity;
        this.outerPoints = outerPoints;
        this.meshFilter = meshFilter;
        this.meshCollider = meshCollider;

        Start();
    }



    void Start() {
        // Setup mesh
        mesh = new Mesh();
        vertCount = (int) math.ceil(math.PI * radius * radius * pointDensity);
        
        // Generate all the points
        verticies = new List<Vector3>();
        int attempts = 0;
        while (verticies.Count < vertCount) {
            float angle = UnityEngine.Random.Range(0, 2 * math.PI);
            float distance = UnityEngine.Random.Range(0, radius);

            float x = math.cos(angle) * distance;
            float z = math.sin(angle) * distance;
            float y = Mathf.PerlinNoise(x * perlinScale, z * perlinScale) * terrainBumpiness.Evaluate(math.clamp((x + radius) / (2 * radius), 0, 1)) * height;
            Vector3 vertex = new Vector3(x, y, z);

            bool tooClose = false;
            foreach (Vector3 existingVertex in verticies) {
                if (Vector3.Distance(existingVertex, vertex) < math.pow(pointDensity * 4, -0.5f)) {
                    tooClose = true;
                    break;
                }
            }

            if (attempts > 25) {
                Debug.Log("TOO MANY VERT ATTEMPTS");
                vertCount = verticies.Count;
                break;
            }

            if (tooClose) {
                attempts += 1;
                continue;
            }

            attempts = 0;
            verticies.Add(vertex);
        }

        for (float angle = 0; angle < 2 * math.PI; angle += 2 * math.PI / outerPoints) {
            float x = math.cos(angle) * radius;
            float z = math.sin(angle) * radius;
            float y = Mathf.PerlinNoise(x * perlinScale, z * perlinScale) * terrainBumpiness.Evaluate(math.clamp((x + radius) / (2 * radius), 0, 1)) * height;
            verticies.Add(new Vector3(x, y, z));

            vertCount += 1;
        }

        // Triangulate the points using the Bowyer-Watson algorithmm
        // Create a super triangle that contains all input points
        radius += 1; // Ensure all points are covered
        Vector3 v1 = new Vector3(2 * math.cos(0) * radius, 0, 2 * math.sin(0) * radius);
        Vector3 v2 = new Vector3(2 * math.cos(2 * math.PI / 3) * radius, 0, 2 * math.sin(2 * math.PI / 3) * radius);
        Vector3 v3 = new Vector3(2 * math.cos(4 * math.PI / 3) * radius, 0, 2 * math.sin(4 * math.PI / 3) * radius);
        radius -= 1; // Radius returned to original value

        verticies.Add(v1);
        verticies.Add(v2);
        verticies.Add(v3);

        triangles = new List<int> {vertCount, vertCount + 1, vertCount + 2};

        // Add each point to the triangulation
        for (int vertIndex = 0; vertIndex < verticies.Count; vertIndex++) {
            Vector3 vert = verticies[vertIndex];

            List<int> badTriangleIndexes = new List<int>();
            List<int> badTriangleEdgeIndexes = new List<int>();

            // Find triangles that are no longer valid with the new point
            for (int index = 0; index < triangles.Count - 2; index += 3) {
                v1 = verticies[triangles[index]];
                v2 = verticies[triangles[index + 1]];
                v3 = verticies[triangles[index + 2]];

                float[][] matrix = new float[3][];
                matrix[0] = new float[] { v1.x - vert.x, v1.z - vert.z, Mathf.Pow(v1.x - vert.x, 2) + Mathf.Pow(v1.z - vert.z, 2) };
                matrix[1] = new float[] { v2.x - vert.x, v2.z - vert.z, Mathf.Pow(v2.x - vert.x, 2) + Mathf.Pow(v2.z - vert.z, 2) };
                matrix[2] = new float[] { v3.x - vert.x, v3.z - vert.z, Mathf.Pow(v3.x - vert.x, 2) + Mathf.Pow(v3.z - vert.z, 2) };

                float det = matrix[0][0] * (matrix[1][1] * matrix[2][2] - matrix[1][2] * matrix[2][1])
                          - matrix[0][1] * (matrix[1][0] * matrix[2][2] - matrix[1][2] * matrix[2][0])
                          + matrix[0][2] * (matrix[1][0] * matrix[2][1] - matrix[1][1] * matrix[2][0]);

                if ((v2.x - v1.x)*(v3.z - v1.z)-(v3.x - v1.x)*(v2.z - v1.z) < 0) {
                    det = -det; // Points are clockwise
                }

                if (det > 0) {
                    badTriangleIndexes.Add(index);
                    badTriangleIndexes.Add(index + 1);
                    badTriangleIndexes.Add(index + 2);

                    badTriangleEdgeIndexes.Add(triangles[index]);
                    badTriangleEdgeIndexes.Add(triangles[index + 1]);
                    badTriangleEdgeIndexes.Add(triangles[index + 1]);
                    badTriangleEdgeIndexes.Add(triangles[index + 2]);
                    badTriangleEdgeIndexes.Add(triangles[index + 2]);
                    badTriangleEdgeIndexes.Add(triangles[index]);
                }
            }

            // Remove all the bad triangles
            List<int> newTriangles = new List<int>();
            for (int index = 0; index < triangles.Count; index ++) {
                if (!badTriangleIndexes.Contains(index)) {
                    newTriangles.Add(triangles[index]);
                }
            }
            triangles = new List<int>(newTriangles);

            // Remove duplicate edges from the polygon
            Dictionary<Tuple<int, int>, int> countedTriangleEdges = new Dictionary<Tuple<int, int>, int>();
            for (int i = 0; i < badTriangleEdgeIndexes.Count - 1; i += 2) {
                int p1 = badTriangleEdgeIndexes[i];
                int p2 = badTriangleEdgeIndexes[i + 1];

                Tuple<int, int> t1 = new Tuple<int, int>(p1, p2);
                Tuple<int, int> t2 = new Tuple<int, int>(p2, p1);

                if (countedTriangleEdges.ContainsKey(t1)) {
                    countedTriangleEdges[t1] += 1;
                } else if (countedTriangleEdges.ContainsKey(t2)) {
                    countedTriangleEdges[t2] += 1;
                } else {
                    countedTriangleEdges[t1] = 1;
                }
            }

            List<int> polygonEdges = new List<int>();
            foreach (var edge in countedTriangleEdges) {
                if (edge.Value == 1) {
                    polygonEdges.Add(edge.Key.Item1);
                    polygonEdges.Add(edge.Key.Item2);
                }
            }

            // Create new triangles from the unique edges and the new point
            for (int index = 0; index < polygonEdges.Count - 1; index += 2) {
                if ((verticies[polygonEdges[index + 1]].x - verticies[polygonEdges[index]].x) * (vert.z - verticies[polygonEdges[index]].z) - (vert.x - verticies[polygonEdges[index]].x) * (verticies[polygonEdges[index + 1]].z - verticies[polygonEdges[index]].z) < 0) {
                    triangles.Add(polygonEdges[index]);
                    triangles.Add(polygonEdges[index + 1]);
                    triangles.Add(vertIndex);
                } else {
                    triangles.Add(polygonEdges[index + 1]);
                    triangles.Add(polygonEdges[index]);
                    triangles.Add(vertIndex);
                }
            }
        }

        // Remove triangles that contain super triangle vertices
        for (int index = 0; index < triangles.Count - 2; index += 3) {
            if (triangles[index] >= vertCount || triangles[index + 1] >= vertCount || triangles[index + 2] >= vertCount) {
                triangles.RemoveAt(index);
                triangles.RemoveAt(index);
                triangles.RemoveAt(index);
                index -= 3;
            }
        }

        // Duplicate every vert per face to get rougher lighting
        List<Vector3> separateVerticies = new List<Vector3>();
        List<int> separateTriangles = new List<int>();
        for (int i = 0; i < triangles.Count; i++) {
            separateVerticies.Add(verticies[triangles[i]]);
            separateTriangles.Add(i);
        }

        verticies =  new List<Vector3>(separateVerticies);
        triangles = new List<int>(separateTriangles);

        // Update the mesh
        mesh.Clear();
        mesh.vertices = verticies.ToArray();
        mesh.triangles = triangles.ToArray();
        mesh.RecalculateNormals();
        mesh.RecalculateTangents();

        meshFilter.mesh = mesh;
        meshCollider.sharedMesh = mesh;
    }
}