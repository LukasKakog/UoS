using Unity.Mathematics;
using UnityEngine;

public class TerrainGenerator : MonoBehaviour {
    public Terrain terrain;
    public float perlinScale = 10f;
    public float maxHeight = 10f;

    public int terrainSizeX = 200;
    public int TerrainSizeY = 1;
    public int terrainSizeZ = 200;

    public GameObject[] trees;
    public GameObject[] bushes;
    public int treeCount = 50;
    public int bushCount = 100;

    public int terrainResolution = 256;

    void Update() {
        // Make the terrain uneven
        terrain.terrainData.heightmapResolution = terrainResolution;
        terrainResolution = terrain.terrainData.heightmapResolution;

        terrain.terrainData.size = new Vector3(terrainSizeX, TerrainSizeY, terrainSizeZ);
        
        float[,] perlinHeights = PerlinHeights();
        terrain.terrainData.SetHeights(0, 0, perlinHeights);

        for (int i = 0; i < treeCount; i++) {
            GameObject tree = trees[UnityEngine.Random.Range(0, trees.Length)];
            Instantiate(tree, getRandomPoint(perlinHeights), Quaternion.identity);
        }

        for (int i = 0; i < bushCount; i++) {
            GameObject bush = bushes[UnityEngine.Random.Range(0, bushes.Length)];
            Instantiate(bush, getRandomPoint(perlinHeights), Quaternion.identity);
        }
    }

    private float[,] PerlinHeights() {
        float[,] allHeights = new float[terrainResolution, terrainResolution];

        for (int x = 0; x < terrainResolution; x += 1) {
            for (int z = 0; z < terrainResolution; z += 1) {
                float xHeight = (float) x / terrainResolution * perlinScale;
                float zHeight = (float) z / terrainResolution * perlinScale;

                float hillyX = math.pow(math.abs(x - (terrainResolution / 2)) / terrainResolution, 2) * 8;
                allHeights[x, z] = Mathf.PerlinNoise(xHeight, zHeight) + hillyX;
            }
        }

        return allHeights;
    }

    private Vector3 getRandomPoint(float[,] perlinHeights) {
        int xHeightPos = UnityEngine.Random.Range(0, perlinHeights.GetLength(0));
        int zHeightPos = UnityEngine.Random.Range(0, perlinHeights.GetLength(1));

        float yPos = perlinHeights[xHeightPos, zHeightPos];
        float xPos = xHeightPos * terrainSizeX / terrainResolution - (terrainSizeX / 2);
        float zPos = zHeightPos * terrainSizeZ / terrainResolution - (terrainSizeZ / 2);

        return new Vector3(zPos, yPos, xPos);
    }
}